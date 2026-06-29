use anyhow::{Context, Result, anyhow};
use clap::Parser;
use std::{
    fs,
    path::{Path, PathBuf},
};

#[derive(Parser, Debug)]
#[command(
    name = "fix-md",
    about = "Repair common OCR-damaged Markdown math delimiters without modifying the source file"
)]
struct Args {
    /// Source Markdown file. This file is read-only from the tool's point of view.
    #[arg(value_name = "INPUT.md")]
    input: PathBuf,

    /// Output Markdown file. Defaults to INPUT_fixed.md beside the source file.
    #[arg(short, long, value_name = "OUTPUT.md")]
    output: Option<PathBuf>,

    /// Allow replacing an existing output file. The input file is still never overwritten.
    #[arg(long)]
    force: bool,
}

#[derive(Debug, Default)]
struct FixReport {
    bracket_display_delimiters: usize,
    latex_display_delimiters: usize,
    dollar_display_blocks: usize,
    one_line_display_blocks: usize,
    removed_blank_math_lines: usize,
    fixed_equals_rules: usize,
    stripped_markdown_headings_in_math: usize,
    escaped_percent_signs: usize,
    trimmed_inline_dollar_math: usize,
    closed_unterminated_display_blocks: usize,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let input = args.input.canonicalize().with_context(|| {
        format!(
            "failed to resolve input path {}; file may not exist",
            args.input.display()
        )
    })?;
    if !input.is_file() {
        return Err(anyhow!("input is not a file: {}", input.display()));
    }

    let output = match args.output {
        Some(path) => absolutize(path)?,
        None => default_output_path(&input)?,
    };

    if output == input {
        return Err(anyhow!(
            "refusing to overwrite source file; choose a different -o path"
        ));
    }
    if output.exists() && !args.force {
        return Err(anyhow!(
            "output already exists: {}\nrerun with --force or choose -o",
            output.display()
        ));
    }

    let source = fs::read_to_string(&input)
        .with_context(|| format!("failed to read {}", input.display()))?;
    let (fixed, report) = fix_markdown(&source);

    if let Some(parent) = output.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    fs::write(&output, fixed).with_context(|| format!("failed to write {}", output.display()))?;

    println!("Read:  {}", input.display());
    println!("Wrote: {}", output.display());
    println!(
        "Math fixes: bracket_delims={}, latex_delims={}, dollar_blocks={}, one_line_blocks={}, blank_math_lines_removed={}, equals_rules_fixed={}, headings_stripped_in_math={}, percents_escaped={}, inline_dollar_spaces_trimmed={}, unterminated_blocks_closed={}",
        report.bracket_display_delimiters,
        report.latex_display_delimiters,
        report.dollar_display_blocks,
        report.one_line_display_blocks,
        report.removed_blank_math_lines,
        report.fixed_equals_rules,
        report.stripped_markdown_headings_in_math,
        report.escaped_percent_signs,
        report.trimmed_inline_dollar_math,
        report.closed_unterminated_display_blocks,
    );

    Ok(())
}

fn absolutize(path: PathBuf) -> Result<PathBuf> {
    if path.is_absolute() {
        Ok(path)
    } else {
        Ok(std::env::current_dir()
            .context("failed to get current directory")?
            .join(path))
    }
}

fn default_output_path(input: &Path) -> Result<PathBuf> {
    let parent = input.parent().unwrap_or_else(|| Path::new("."));
    let stem = input
        .file_stem()
        .and_then(|value| value.to_str())
        .ok_or_else(|| anyhow!("input file name is not valid UTF-8: {}", input.display()))?;
    Ok(parent.join(format!("{stem}_fixed.md")))
}

fn fix_markdown(source: &str) -> (String, FixReport) {
    let mut report = FixReport::default();
    let mut output = Vec::<String>::new();
    let mut block = Vec::<String>::new();
    let mut in_display_math = false;
    let mut source_had_trailing_newline = false;

    for line in source.split_inclusive('\n') {
        source_had_trailing_newline = line.ends_with('\n');
        let line = line.strip_suffix('\n').unwrap_or(line);
        let line = line.strip_suffix('\r').unwrap_or(line);
        let trimmed = line.trim();

        if in_display_math {
            if is_display_close(trimmed) {
                push_math_block(&mut output, &block, &mut report);
                block.clear();
                in_display_math = false;
            } else {
                block.push(line.to_string());
            }
            continue;
        }

        match display_open_delimiter(trimmed) {
            Some(DisplayDelimiter::Bracket) => {
                report.bracket_display_delimiters += 1;
                in_display_math = true;
            }
            Some(DisplayDelimiter::Latex) => {
                report.latex_display_delimiters += 1;
                in_display_math = true;
            }
            Some(DisplayDelimiter::Dollar) => {
                report.dollar_display_blocks += 1;
                in_display_math = true;
            }
            None => {
                if let Some(content) = one_line_display_content(trimmed) {
                    report.one_line_display_blocks += 1;
                    push_math_block(&mut output, &[content.to_string()], &mut report);
                } else if let Some(content) = one_line_latex_display_content(trimmed) {
                    report.latex_display_delimiters += 2;
                    report.one_line_display_blocks += 1;
                    push_math_block(&mut output, &[content.to_string()], &mut report);
                } else {
                    let (fixed_line, count) = normalize_inline_dollar_math(line);
                    report.trimmed_inline_dollar_math += count;
                    output.push(fixed_line);
                }
            }
        }
    }

    if in_display_math {
        report.closed_unterminated_display_blocks += 1;
        push_math_block(&mut output, &block, &mut report);
    }

    let mut fixed = output.join("\n");
    if source_had_trailing_newline {
        fixed.push('\n');
    }
    (fixed, report)
}

#[derive(Clone, Copy, Debug)]
enum DisplayDelimiter {
    Bracket,
    Latex,
    Dollar,
}

fn display_open_delimiter(trimmed: &str) -> Option<DisplayDelimiter> {
    match trimmed {
        "[" => Some(DisplayDelimiter::Bracket),
        r"\[" => Some(DisplayDelimiter::Latex),
        "$$" => Some(DisplayDelimiter::Dollar),
        _ => None,
    }
}

fn is_display_close(trimmed: &str) -> bool {
    matches!(trimmed, "]" | r"\]" | "$$")
}

fn one_line_display_content(trimmed: &str) -> Option<&str> {
    if trimmed.len() >= 4 && trimmed.starts_with("$$") && trimmed.ends_with("$$") {
        let inner = &trimmed[2..trimmed.len() - 2];
        Some(inner.trim())
    } else {
        None
    }
}

fn one_line_latex_display_content(trimmed: &str) -> Option<&str> {
    if trimmed.len() >= 4 && trimmed.starts_with(r"\[") && trimmed.ends_with(r"\]") {
        let inner = &trimmed[2..trimmed.len() - 2];
        Some(inner.trim())
    } else {
        None
    }
}

fn push_math_block(output: &mut Vec<String>, raw_lines: &[String], report: &mut FixReport) {
    output.push("$$".to_string());
    for line in fix_math_lines(raw_lines, report) {
        output.push(line);
    }
    output.push("$$".to_string());
}

fn fix_math_lines(raw_lines: &[String], report: &mut FixReport) -> Vec<String> {
    let mut lines = Vec::<String>::new();

    for raw in raw_lines {
        let mut line = raw.trim().to_string();
        if line.is_empty() {
            report.removed_blank_math_lines += 1;
            continue;
        }
        if let Some(stripped) = strip_markdown_heading_marker(&line) {
            report.stripped_markdown_headings_in_math += 1;
            line = stripped.to_string();
        }
        let (escaped, count) = escape_unescaped_percent(&line);
        report.escaped_percent_signs += count;
        lines.push(escaped);
    }

    let mut fixed = Vec::<String>::new();
    let mut index = 0;
    while index < lines.len() {
        let line = lines[index].trim();
        if is_equals_rule(line) && !fixed.is_empty() && index + 1 < lines.len() {
            let previous = fixed.pop().unwrap();
            let next = lines[index + 1].trim();
            fixed.push(format!("{}={}", previous.trim_end(), next.trim_start()));
            report.fixed_equals_rules += 1;
            index += 2;
        } else {
            fixed.push(line.to_string());
            index += 1;
        }
    }

    fixed
}

fn strip_markdown_heading_marker(line: &str) -> Option<&str> {
    let trimmed = line.trim_start();
    let hashes = trimmed.chars().take_while(|&ch| ch == '#').count();
    if hashes == 0 {
        return None;
    }
    let rest = &trimmed[hashes..];
    if rest.starts_with(char::is_whitespace) {
        Some(rest.trim_start())
    } else {
        None
    }
}

fn is_equals_rule(line: &str) -> bool {
    let chars = line.chars();
    let mut count = 0usize;
    for ch in chars {
        if ch != '=' {
            return false;
        }
        count += 1;
    }
    count >= 2
}

fn escape_unescaped_percent(line: &str) -> (String, usize) {
    let mut escaped = String::with_capacity(line.len());
    let mut previous_was_backslash = false;
    let mut count = 0usize;

    for ch in line.chars() {
        if ch == '%' && !previous_was_backslash {
            escaped.push('\\');
            count += 1;
        }
        escaped.push(ch);
        previous_was_backslash = ch == '\\' && !previous_was_backslash;
        if ch != '\\' {
            previous_was_backslash = false;
        }
    }

    (escaped, count)
}

fn normalize_inline_dollar_math(line: &str) -> (String, usize) {
    let mut output = String::with_capacity(line.len());
    let mut cursor = 0usize;
    let mut trimmed_count = 0usize;

    while let Some(open) = find_single_dollar(line, cursor) {
        let Some(close) = find_single_dollar(line, open + 1) else {
            break;
        };

        output.push_str(&line[cursor..open]);
        let inner = &line[open + 1..close];
        let trimmed = inner.trim();
        if trimmed.len() != inner.len() {
            trimmed_count += 1;
        }
        output.push('$');
        output.push_str(trimmed);
        output.push('$');
        cursor = close + 1;
    }

    if cursor == 0 {
        (line.to_string(), 0)
    } else {
        output.push_str(&line[cursor..]);
        (output, trimmed_count)
    }
}

fn find_single_dollar(line: &str, start: usize) -> Option<usize> {
    for (offset, ch) in line[start..].char_indices() {
        if ch != '$' {
            continue;
        }

        let index = start + offset;
        if is_escaped(line, index) {
            continue;
        }
        if line[..index].ends_with('$') || line[index + 1..].starts_with('$') {
            continue;
        }
        return Some(index);
    }
    None
}

fn is_escaped(line: &str, index: usize) -> bool {
    let mut backslashes = 0usize;
    for ch in line[..index].chars().rev() {
        if ch == '\\' {
            backslashes += 1;
        } else {
            break;
        }
    }
    backslashes % 2 == 1
}

#[cfg(test)]
mod tests {
    use super::fix_markdown;

    #[test]
    fn converts_bracket_display_math() {
        let input = "before\n[\na+b\n]\nafter\n";
        let (fixed, _) = fix_markdown(input);
        assert_eq!(fixed, "before\n$$\na+b\n$$\nafter\n");
    }

    #[test]
    fn repairs_ocr_equals_rule_inside_math() {
        let input = "[\nY-Y_P\n=====\n-\\frac{P_X}{P_Y}(X-X_P)\n]\n";
        let (fixed, _) = fix_markdown(input);
        assert_eq!(fixed, "$$\nY-Y_P=-\\frac{P_X}{P_Y}(X-X_P)\n$$\n");
    }

    #[test]
    fn escapes_percent_inside_math() {
        let input = "$$\n3.33%\n$$\n";
        let (fixed, _) = fix_markdown(input);
        assert_eq!(fixed, "$$\n3.33\\%\n$$\n");
    }

    #[test]
    fn trims_spaces_inside_inline_dollar_math() {
        let input = "目标函数为： $ \\min z = 4x_{1} - 3x_{2} $。\n";
        let (fixed, _) = fix_markdown(input);
        assert_eq!(fixed, "目标函数为： $\\min z = 4x_{1} - 3x_{2}$。\n");
    }

    #[test]
    fn does_not_treat_display_dollars_as_inline_math() {
        let input = "$$ x+1 $$\n";
        let (fixed, _) = fix_markdown(input);
        assert_eq!(fixed, "$$\nx+1\n$$\n");
    }
}
