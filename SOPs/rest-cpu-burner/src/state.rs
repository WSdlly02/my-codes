use std::sync::{
    Arc,
    atomic::{AtomicU8, AtomicU32},
};

#[derive(Clone)]
pub struct AppState {
    /// 用户设定的"系统总 CPU 目标使用率"（0-100）
    pub target: Arc<AtomicU8>,
    /// 采样得到的当前系统 CPU 使用率（百分比 * PERCENT_SCALE）
    pub system_usage: Arc<AtomicU32>,
    /// 平滑后的系统 CPU 使用率（百分比 * PERCENT_SCALE）
    pub smoothed_usage: Arc<AtomicU32>,
    /// 扣除本进程估算消耗后的外部 CPU 使用率（百分比 * PERCENT_SCALE）
    pub external_usage: Arc<AtomicU32>,
    /// 当前下发给 worker 的进程级占用率（百分比 * PERCENT_SCALE）
    pub current_burn: Arc<AtomicU32>,
    pub access_token: Arc<String>,
}

impl AppState {
    pub fn new(access_token: String) -> Self {
        Self {
            target: Arc::new(AtomicU8::new(0)),
            system_usage: Arc::new(AtomicU32::new(0)),
            smoothed_usage: Arc::new(AtomicU32::new(0)),
            external_usage: Arc::new(AtomicU32::new(0)),
            current_burn: Arc::new(AtomicU32::new(0)),
            access_token: Arc::new(access_token),
        }
    }
}
