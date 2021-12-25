use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let levels: Vec<Vec<u8>> = args
        .iter()
        .map(|x| {
            x.chars()
                .map(|y| y.to_digit(10).unwrap() as u8)
                .collect::<Vec<u8>>()
        })
        .collect();
    let dim = (levels.len(), levels[0].len());

    let mut counter: u64 = 0;

    for (i, line) in levels.iter().enumerate() {
        for (j, level) in line.iter().enumerate() {
            if (i <= 0 || levels[i - 1][j] > *level)
                && (i >= dim.0 - 1 || levels[i + 1][j] > *level)
                && (j <= 0 || levels[i][j - 1] > *level)
                && (j >= dim.1 - 1 || levels[i][j + 1] > *level)
            {
                counter += (level + 1) as u64;
            }
        }
    }

    println!("{}", counter);
}
