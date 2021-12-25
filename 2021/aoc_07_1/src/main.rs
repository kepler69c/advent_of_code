use std::io::BufRead;

fn dist(a: u64, b: u64) -> u64 {
    let min = std::cmp::min(a, b);
    let max = std::cmp::max(a, b);
    return max - min;
}

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let crabs: Vec<u64> = args[0]
        .split(',')
        .map(|x| x.parse::<u64>().unwrap())
        .collect();

    let min = crabs.iter().min().unwrap();
    let max = crabs.iter().max().unwrap();

    let mut costs = Vec::new();
    for i in *min..(*max + 1) {
        let mut sum = 0;
        for c in crabs.iter() {
            sum += dist(*c, i);
        }
        costs.push(sum);
    }

    println!("{}", costs.iter().min().unwrap());
}
