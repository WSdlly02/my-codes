use rest_cpu_burner::{api, sampler, state::AppState, worker};
use tokio::sync::broadcast;

#[tokio::main]
async fn main() {
    let access_token = match std::env::var("ACCESS_TOKEN") {
        Ok(token) => token,
        Err(_) => {
            eprintln!("Set the ACCESS_TOKEN environment variable");
            std::process::exit(1)
        }
    };
    let bind_addr = match std::env::var("BIND_ADDR") {
        Ok(addr) => addr,
        Err(_) => {
            eprintln!("Set the BIND_ADDR environment variable");
            std::process::exit(1)
        }
    };

    let state = AppState::new(access_token);
    let (tx, _keep_alive) = broadcast::channel::<u32>(64);

    let cpu_count = num_cpus::get();
    println!("[INFO] detected {} CPU(s)", cpu_count);

    worker::start(cpu_count, &tx);
    sampler::start(state.clone(), tx);

    println!("[INFO] CPU Burner listening on http://{}", bind_addr);
    let listener = tokio::net::TcpListener::bind(&bind_addr)
        .await
        .expect("failed to bind");
    axum::serve(listener, api::router(state))
        .await
        .expect("server error");
}
