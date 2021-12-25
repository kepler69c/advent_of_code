use std::collections::HashSet;
use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let (dots, folds): (Vec<&String>, Vec<&String>) = {
        let mut s = args.split(|x| x == "");
        (
            s.next().unwrap().iter().collect(),
            s.next().unwrap().iter().collect(),
        )
    };

    let dots: Vec<(u32, u32)> = dots
        .iter()
        .map(|x| {
            let mut s = x.split(',');
            (
                s.next().unwrap().parse::<u32>().unwrap(),
                s.next().unwrap().parse::<u32>().unwrap(),
            )
        })
        .collect();

    let mut dots: HashSet<(u32, u32)> = dots.into_iter().collect();

    let (fold_x, fold_y): (Vec<Option<u32>>, Vec<Option<u32>>) = folds
        .iter()
        .map(|x| {
            let mut s = x.split('=');
            if s.next().unwrap().chars().last().unwrap() == 'x' {
                (Some(s.next().unwrap().parse::<u32>().unwrap()), None)
            } else {
                (None, Some(s.next().unwrap().parse::<u32>().unwrap()))
            }
        })
        .unzip();

    let (fold_x, fold_y): (Vec<u32>, Vec<u32>) = (
        fold_x.into_iter().flatten().collect(),
        fold_y.into_iter().flatten().collect(),
    );

    for &fold in fold_x.iter() {
        let new_dots: HashSet<(u32, u32)> = dots
            .iter()
            .map(|&(x, y)| (if x > fold { fold - (x - fold) } else { x }, y))
            .collect();
        dots = new_dots;
    }

    for &fold in fold_y.iter() {
        let new_dots: HashSet<(u32, u32)> = dots
            .iter()
            .map(|&(x, y)| (x, if y > fold { fold - (y - fold) } else { y }))
            .collect();
        dots = new_dots;
    }

    let min_x: u32 = dots.iter().map(|x| x.0).min().unwrap();
    let min_y: u32 = dots.iter().map(|x| x.1).min().unwrap();
    let max_x: u32 = dots.iter().map(|x| x.0).max().unwrap();
    let max_y: u32 = dots.iter().map(|x| x.1).max().unwrap();

    for y in min_y..(max_y + 1) {
        for x in min_x..(max_x + 1) {
            print!("{}", if dots.contains(&(x, y)) { '#' } else { '.' });
        }
        print!("\n");
    }
}
