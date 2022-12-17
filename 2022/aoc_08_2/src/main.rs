use std::io::BufRead;

fn main() {
    let args = std::io::stdin().lock().lines().flatten();
    let trees: Vec<Vec<_>> = args
        .map(|x| x.chars().map(|x| x.to_digit(10).unwrap()).collect())
        .collect();
    let dim = (trees.len(), trees[0].len());

    let mut max_scenic = 0;

    (0..dim.0).for_each(|y| {
        (0..dim.1).for_each(|x| {
            let mut count = (0, 0, 0, 0);

            // look up
            (0..y).rev().try_for_each(|i| {
                count.0 += 1;
                if trees[y][x] > trees[i][x] {
                    Some(())
                } else {
                    None
                }
            });
            // look down
            (y + 1..dim.0).try_for_each(|i| {
                count.1 += 1;
                if trees[y][x] > trees[i][x] {
                    Some(())
                } else {
                    None
                }
            });
            // look left
            (0..x).rev().try_for_each(|i| {
                count.2 += 1;
                if trees[y][x] > trees[y][i] {
                    Some(())
                } else {
                    None
                }
            });
            // look right
            (x + 1..dim.1).try_for_each(|i| {
                count.3 += 1;
                if trees[y][x] > trees[y][i] {
                    Some(())
                } else {
                    None
                }
            });

            max_scenic = max_scenic.max(count.0 * count.1 * count.2 * count.3);
        });
    });

    println!("{}", max_scenic);
}
