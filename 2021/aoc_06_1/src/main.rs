use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let mut fishes: Vec<u64> = args[0]
        .split(',')
        .map(|x| x.parse::<u64>().unwrap())
        .collect();

    for _ in 0..80 {
        let mut new_fishes: Vec<u64> = Vec::new();
        for fish in fishes.iter() {
            if *fish == 0 {
                new_fishes.push(6);
                new_fishes.push(8);
            } else {
                new_fishes.push(*fish - 1);
            }
        }
        fishes = new_fishes.clone();
    }

    println!("{}", fishes.len());
}
