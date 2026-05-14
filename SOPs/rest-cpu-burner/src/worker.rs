use std::time::{Duration, Instant};

use tokio::sync::broadcast;

use crate::control::PERCENT_SCALE;

const WORKER_SLICE: Duration = Duration::from_millis(5);

pub fn start(cpu_count: usize, tx: &broadcast::Sender<u32>) {
    for _id in 0..cpu_count {
        let mut rx = tx.subscribe();
        std::thread::spawn(move || {
            let slice_ns = WORKER_SLICE.as_nanos() as u64;
            let mut burn_bp: u32 = 0;
            let mut remainder: u64 = 0;

            loop {
                while let Ok(p) = rx.try_recv() {
                    burn_bp = p;
                }

                let scaled_burn_ns = slice_ns * u64::from(burn_bp) + remainder;
                let burn_ns = scaled_burn_ns / (100 * u64::from(PERCENT_SCALE));
                remainder = scaled_burn_ns % (100 * u64::from(PERCENT_SCALE));
                let sleep_ns = slice_ns - burn_ns;

                if burn_ns > 0 {
                    let deadline = Instant::now() + Duration::from_nanos(burn_ns);
                    while Instant::now() < deadline {
                        std::hint::spin_loop();
                    }
                }

                if sleep_ns > 0 {
                    std::thread::sleep(Duration::from_nanos(sleep_ns));
                }
            }
        });
    }
}
