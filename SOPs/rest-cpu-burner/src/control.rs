pub const PERCENT_SCALE: u32 = 100;

const EWMA_ALPHA: f64 = 0.25;
const CONTROL_KP: f64 = 0.45;
const MAX_BURN_STEP_PERCENT: f64 = 8.0;

#[derive(Clone, Copy)]
pub struct ControlSample {
    pub raw_system_bp: u32,
    pub smoothed_system_bp: u32,
    pub external_bp: u32,
    pub burn_bp: u32,
}

pub fn percent_to_bp(percent: f64) -> u32 {
    (percent.clamp(0.0, 100.0) * PERCENT_SCALE as f64).round() as u32
}

pub fn bp_to_percent(bp: u32) -> f64 {
    bp as f64 / PERCENT_SCALE as f64
}

fn apply_ewma(previous_bp: Option<u32>, measured_bp: u32) -> u32 {
    let Some(previous_bp) = previous_bp else {
        return measured_bp;
    };

    percent_to_bp(
        EWMA_ALPHA * bp_to_percent(measured_bp) + (1.0 - EWMA_ALPHA) * bp_to_percent(previous_bp),
    )
}

fn estimate_external_usage(system_bp: u32, current_burn_bp: u32) -> u32 {
    system_bp.saturating_sub(current_burn_bp)
}

fn limit_step(previous_bp: u32, next_bp: u32, max_step_bp: u32) -> u32 {
    if next_bp > previous_bp {
        next_bp.min(previous_bp + max_step_bp)
    } else {
        next_bp.max(previous_bp.saturating_sub(max_step_bp))
    }
}

fn next_burn_bp(target_percent: u8, measured_system_bp: u32, previous_burn_bp: u32) -> u32 {
    if target_percent == 0 {
        return 0;
    }

    let target_bp = u32::from(target_percent) * PERCENT_SCALE;
    let error = target_bp as f64 - measured_system_bp as f64;
    let requested_bp = (previous_burn_bp as f64 + CONTROL_KP * error)
        .round()
        .clamp(0.0, 100.0 * PERCENT_SCALE as f64) as u32;

    limit_step(
        previous_burn_bp,
        requested_bp,
        percent_to_bp(MAX_BURN_STEP_PERCENT),
    )
}

pub fn update_control(
    target_percent: u8,
    raw_system_bp: u32,
    previous_smoothed_bp: Option<u32>,
    previous_burn_bp: u32,
) -> ControlSample {
    let smoothed_system_bp = apply_ewma(previous_smoothed_bp, raw_system_bp);
    let external_bp = estimate_external_usage(smoothed_system_bp, previous_burn_bp);
    let burn_bp = next_burn_bp(target_percent, smoothed_system_bp, previous_burn_bp);

    ControlSample {
        raw_system_bp,
        smoothed_system_bp,
        external_bp,
        burn_bp,
    }
}

#[cfg(test)]
mod tests {
    use super::{
        apply_ewma, bp_to_percent, estimate_external_usage, limit_step, next_burn_bp,
        percent_to_bp, update_control,
    };

    #[test]
    fn basis_points_preserve_fractional_percent() {
        assert_eq!(percent_to_bp(70.25), 7025);
        assert_eq!(bp_to_percent(7025), 70.25);
    }

    #[test]
    fn external_usage_saturates_at_zero() {
        assert_eq!(estimate_external_usage(6500, 4000), 2500);
        assert_eq!(estimate_external_usage(2000, 4000), 0);
    }

    #[test]
    fn ewma_uses_first_sample_directly() {
        assert_eq!(apply_ewma(None, 6200), 6200);
    }

    #[test]
    fn burn_step_is_rate_limited() {
        assert_eq!(limit_step(2000, 9000, 400), 2400);
        assert_eq!(limit_step(9000, 2000, 400), 8600);
    }

    #[test]
    fn proportional_control_moves_toward_target() {
        let burn = next_burn_bp(70, 6200, 6000);
        assert!(burn > 6000);
        assert!(burn <= 6800);
    }

    #[test]
    fn update_control_smooths_and_adjusts_burn() {
        let sample = update_control(70, 6200, Some(7000), 6800);
        assert!(sample.smoothed_system_bp < 7000);
        assert!(sample.burn_bp > 6800);
        assert!(sample.burn_bp <= 7600);
    }
}
