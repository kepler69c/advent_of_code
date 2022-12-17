use std::io::BufRead;

fn main() {
    let args = std::io::stdin().lock().lines().flatten();
    let mut trees: Vec<Vec<_>> = args
        .map(|x| {
            x.chars()
                .map(|x| (x.to_digit(10).unwrap(), false))
                .collect()
        })
        .collect();
    let dim = (trees.len(), trees[0].len());
    let mut count = 0;

    // top->bottom
    let mut maxes: Vec<_> = (0..dim.1)
        .map(|i| {
            if !trees[0][i].1 {
                count += 1;
                trees[0][i].1 = true;
            }
            trees[0][i].0
        })
        .collect();
    (1..dim.0).for_each(|y| {
        (0..dim.1).for_each(|x| {
            if trees[y][x].0 > maxes[x] {
                maxes[x] = trees[y][x].0;
                if !trees[y][x].1 {
                    count += 1;
                    trees[y][x].1 = true;
                }
            }
        })
    });

    // bottom->top
    let mut maxes: Vec<_> = (0..dim.1)
        .map(|i| {
            if !trees[dim.0 - 1][i].1 {
                count += 1;
                trees[dim.0 - 1][i].1 = true;
            }
            trees[dim.0 - 1][i].0
        })
        .collect();
    (0..dim.0 - 1).rev().for_each(|y| {
        (0..dim.1).for_each(|x| {
            if trees[y][x].0 > maxes[x] {
                maxes[x] = trees[y][x].0;
                if !trees[y][x].1 {
                    count += 1;
                    trees[y][x].1 = true;
                }
            }
        })
    });

    // left->right
    let mut maxes: Vec<_> = (0..dim.0)
        .map(|i| {
            if !trees[i][0].1 {
                count += 1;
                trees[i][0].1 = true;
            }
            trees[i][0].0
        })
        .collect();
    (1..dim.1).for_each(|x| {
        (0..dim.0).for_each(|y| {
            if trees[y][x].0 > maxes[y] {
                maxes[y] = trees[y][x].0;
                if !trees[y][x].1 {
                    count += 1;
                    trees[y][x].1 = true;
                }
            }
        })
    });

    // right->left
    let mut maxes: Vec<_> = (0..dim.0)
        .map(|i| {
            if !trees[i][dim.1 - 1].1 {
                count += 1;
                trees[i][dim.1 - 1].1 = true;
            }
            trees[i][dim.1 - 1].0
        })
        .collect();
    (0..dim.1 - 1).rev().for_each(|x| {
        (0..dim.0).for_each(|y| {
            if trees[y][x].0 > maxes[y] {
                maxes[y] = trees[y][x].0;
                if !trees[y][x].1 {
                    count += 1;
                    trees[y][x].1 = true;
                }
            }
        })
    });

    println!("{}", count);
}
