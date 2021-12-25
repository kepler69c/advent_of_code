use std::collections::BinaryHeap;
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

    let mut basins = vec![vec![false; dim.1]; dim.0];
    let mut sizes = BinaryHeap::new();

    for (i, line) in levels.iter().enumerate() {
        for (j, level) in line.iter().enumerate() {
            if !basins[i][j]
                && (i <= 0 || levels[i - 1][j] > *level)
                && (i >= dim.0 - 1 || levels[i + 1][j] > *level)
                && (j <= 0 || levels[i][j - 1] > *level)
                && (j >= dim.1 - 1 || levels[i][j + 1] > *level)
            {
                // DFS
                let mut queue = Vec::new();
                let root = (i, j);
                basins[root.0][root.1] = true;
                queue.push(root);
                let mut counter = 0;

                while queue.len() > 0 {
                    counter += 1;
                    let v = queue.pop().unwrap();
                    let (i, j) = v;
                    if i > 0 && levels[i - 1][j] != 9 && !basins[i - 1][j] {
                        queue.push((i - 1, j));
                        basins[i - 1][j] = true;
                    }
                    if i < dim.0 - 1 && levels[i + 1][j] != 9 && !basins[i + 1][j] {
                        queue.push((i + 1, j));
                        basins[i + 1][j] = true;
                    }
                    if j > 0 && levels[i][j - 1] != 9 && !basins[i][j - 1] {
                        queue.push((i, j - 1));
                        basins[i][j - 1] = true;
                    }
                    if j < dim.1 - 1 && levels[i][j + 1] != 9 && !basins[i][j + 1] {
                        queue.push((i, j + 1));
                        basins[i][j + 1] = true;
                    }
                }

                sizes.push(counter);
            }
        }
    }

    println!(
        "{}",
        sizes
            .into_sorted_vec()
            .iter()
            .rev()
            .take(3)
            .fold(1, |acc, x| acc * x)
    );
}
