use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let numbers: Vec<u32> = args[0]
        .split(',')
        .map(|x| x.parse::<u32>().unwrap())
        .collect();

    let mut boards: Vec<Vec<Vec<u32>>> = Vec::new();
    let mut line_index = 2;
    let mut nb_boards = 0;
    while line_index + 4 < args.len() {
        boards.push(Vec::new());
        boards[nb_boards].push(
            args[line_index]
                .split_ascii_whitespace()
                .map(|x| x.parse::<u32>().unwrap())
                .collect(),
        );
        boards[nb_boards].push(
            args[line_index + 1]
                .split_ascii_whitespace()
                .map(|x| x.parse::<u32>().unwrap())
                .collect(),
        );
        boards[nb_boards].push(
            args[line_index + 2]
                .split_ascii_whitespace()
                .map(|x| x.parse::<u32>().unwrap())
                .collect(),
        );
        boards[nb_boards].push(
            args[line_index + 3]
                .split_ascii_whitespace()
                .map(|x| x.parse::<u32>().unwrap())
                .collect(),
        );
        boards[nb_boards].push(
            args[line_index + 4]
                .split_ascii_whitespace()
                .map(|x| x.parse::<u32>().unwrap())
                .collect(),
        );

        line_index += 6;
        nb_boards += 1;
    }

    let mut marked_boards: Vec<Vec<Vec<bool>>> = vec![vec![vec![false; 5]; 5]; nb_boards];
    let mut board_wins = vec![false; nb_boards];
    // (board_index, number, boards)
    let mut win_state: (usize, u32, Option<Vec<Vec<Vec<bool>>>>) = (0, 0, None);
    for number in numbers {
        for k in 0..25 {
            let (i, j) = (k % 5, k / 5);
            for b_i in 0..nb_boards {
                if boards[b_i][i][j] == number {
                    marked_boards[b_i][i][j] = true;
                }
            }
        }
        // lines
        for b_i in 0..nb_boards {
            if board_wins[b_i] {
                continue;
            }
            for i in 0..5 {
                let mut completed_line = true;
                for j in 0..5 {
                    if !marked_boards[b_i][i][j] {
                        completed_line = false;
                        break;
                    }
                }
                if completed_line {
                    win_state = (b_i, number, Some(marked_boards.clone()));
                    board_wins[b_i] = true;
                    break;
                }
            }
        }
        // columns
        for b_i in 0..nb_boards {
            if board_wins[b_i] {
                continue;
            }
            for i in 0..5 {
                let mut completed_col = true;
                for j in 0..5 {
                    if !marked_boards[b_i][j][i] {
                        completed_col = false;
                        break;
                    }
                }
                if completed_col {
                    win_state = (b_i, number, Some(marked_boards.clone()));
                    board_wins[b_i] = true;
                    break;
                }
            }
        }
        let mut end = true;
        for b in board_wins.iter() {
            if !b {
                end = false;
                break;
            }
        }
        if end {
            break;
        }
    }
    let mut sum = 0;
    let (b_i, number, _) = win_state;
    let marked_boards = win_state.2.unwrap();
    for k in 0..25 {
        let (ii, ij) = (k % 5, k / 5);
        if !marked_boards[b_i][ii][ij] {
            sum += boards[b_i][ii][ij];
        }
    }
    println!("{}", sum * number);
}
