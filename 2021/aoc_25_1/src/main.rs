use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let mut map: Vec<Vec<char>> = args.iter().map(|x| x.chars().collect()).collect();
    let mut count = 0;

    loop {
        let mut map_copy = vec![vec!['.'; map[0].len()]; map.len()];
        for i in 0..map.len() {
            for j in (0..map[0].len()).rev() {
                if map[i][j] == '>' {
                    if map[i][(j + 1) % map[0].len()] == '.'
                        && map_copy[i][(j + 1) % map[0].len()] == '.'
                    {
                        map_copy[i][(j + 1) % map[0].len()] = '>';
                    } else {
                        map_copy[i][j] = '>';
                    }
                }
            }
        }
        for i in (0..map.len()).rev() {
            for j in 0..map[0].len() {
                if map[i][j] == 'v' {
                    if map[(i + 1) % map.len()][j] != 'v' && map_copy[(i + 1) % map.len()][j] == '.'
                    {
                        map_copy[(i + 1) % map.len()][j] = 'v';
                    } else {
                        map_copy[i][j] = 'v';
                    }
                }
            }
        }
        count += 1;
        if map == map_copy {
            break;
        }
        map = map_copy;
    }

    println!("{}", count);
}
