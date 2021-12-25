use std::collections::HashSet;
use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let (dots, mut folds): (Vec<&String>, Vec<&String>) = {
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

    folds.drain(1..);

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

    println!("{}", dots.len());
}
