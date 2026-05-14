use std::{sync::atomic::Ordering, time::Duration};

use sysinfo::System;
use tokio::sync::broadcast;

use crate::{
    control::{percent_to_bp, update_control},
    state::AppState,
};

const SAMPLE_INTERVAL: Duration = Duration::from_millis(500);

pub fn start(state: AppState, tx: broadcast::Sender<u32>) {
    std::thread::spawn(move || {
        let mut sys = System::new();
        let mut previous_smoothed_bp = None;

        // sysinfo 要求先刷新一次才能得到有效数据。
        sys.refresh_cpu_usage();
        std::thread::sleep(SAMPLE_INTERVAL);

        loop {
            sys.refresh_cpu_usage();

            let raw_system_bp = percent_to_bp(f64::from(sys.global_cpu_usage()));
            let sample = update_control(
                state.target.load(Ordering::Relaxed),
                raw_system_bp,
                previous_smoothed_bp,
                state.current_burn.load(Ordering::Relaxed),
            );
            previous_smoothed_bp = Some(sample.smoothed_system_bp);

            state
                .system_usage
                .store(sample.raw_system_bp, Ordering::Relaxed);
            state
                .smoothed_usage
                .store(sample.smoothed_system_bp, Ordering::Relaxed);
            state
                .external_usage
                .store(sample.external_bp, Ordering::Relaxed);
            state.current_burn.store(sample.burn_bp, Ordering::Relaxed);

            let _ = tx.send(sample.burn_bp);

            std::thread::sleep(SAMPLE_INTERVAL);
        }
    });
}
