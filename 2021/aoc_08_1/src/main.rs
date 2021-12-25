use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let digits: Vec<&str> = args
        .iter()
        .map(|x| x.split('|').skip(1).next().unwrap())
        .map(|x| x.split_whitespace())
        .flatten()
        .collect();

    let count = digits.iter().fold(0, |acc, x| {
        if x.len() == 2 || x.len() == 3 || x.len() == 4 || x.len() == 7 {
            acc + 1
        } else {
            acc
        }
    });

    println!("{}", count);
}
