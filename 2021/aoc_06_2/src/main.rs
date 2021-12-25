use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let fishes: Vec<u64> = args[0]
        .split(',')
        .map(|x| x.parse::<u64>().unwrap())
        .collect();

    let mut array_8: [u64; 9] = [0; 9];
    for fish in fishes {
        array_8[fish as usize] += 1;
    }

    for _ in 0..256 {
        let mut new_array_8: [u64; 9] = [0; 9];
        new_array_8[8] = array_8[0];
        new_array_8[6] = array_8[0];
        for i in 1..9 {
            new_array_8[i - 1] += array_8[i];
        }
        array_8 = new_array_8.clone();
    }

    println!("{}", array_8.iter().sum::<u64>());
}
