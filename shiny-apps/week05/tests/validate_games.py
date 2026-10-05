"""Verify historical scores, move legality, and R board updates.

Requires python-chess (test dependency only) and Rscript:
  python validate_games.py --rscript /path/to/Rscript
"""
import argparse
from pathlib import Path
import re
import subprocess
import tempfile

import chess
from scores import RECORDS


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rscript", default="Rscript")
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[3]
    app = root / "shiny-apps/week05/app.R"
    text = app.read_text(encoding="utf-8")
    assert text == (root / "shiny-apps-dev/week05/app.R").read_text(encoding="utf-8")
    games = text.split("GAMES <- list(", 1)[1].split("GAME_CHOICES <-", 1)[0]
    blocks = re.findall(r"moves = strsplit\(\s*paste\((.*?)\),\s*\" \"\s*\)\[\[1\]\]", games, re.S)
    assert len(blocks) == len(RECORDS) == 5
    expected = []
    for index, (block, (source, score)) in enumerate(zip(blocks, RECORDS), 1):
        moves = " ".join(re.findall(r'"([^"]*)"', block)).split()
        historical = chess.Board()
        historical_moves = []
        for san in score.split():
            move = historical.parse_san(san)
            historical_moves.append(move.uci())
            historical.push(move)
        assert moves == historical_moves, f"Game {index} differs from {source}"
        board = chess.Board()
        for step, uci in enumerate([None] + moves):
            if uci:
                move = chess.Move.from_uci(uci)
                assert move in board.legal_moves, (index, step, uci)
                board.push(move)
            pieces = []
            for rank in range(7, -1, -1):
                for file in range(8):
                    piece = board.piece_at(chess.square(file, rank))
                    pieces.append("" if piece is None else
                                  ("w" if piece.color else "b") + piece.symbol().upper())
            expected.append(f"{index}:{step}:" + ",".join(pieces))
        if index <= 3:
            assert board.is_checkmate(), f"Game {index} must end in mate"
        print(f"Game {index}: {len(moves)} legal plies, matches published score")
    # Exercise the actual R move applier, without requiring Shiny or renv.
    r_code = r'''
env <- new.env()
for (e in parse(commandArgs(TRUE)[1])) {
  if (is.call(e) && identical(e[[1]], as.name("<-"))) {
    name <- as.character(e[[2]])
    if (!name %in% c("ui", "server")) eval(e, env)
  }
}
for (i in seq_along(env$GAMES)) {
  board <- env$initial_board()
  cat(i, ":0:", paste(as.vector(t(board)), collapse=","), "\n", sep="")
  for (step in seq_along(env$GAMES[[i]]$moves)) {
    board <- env$apply_uci_move(board, env$GAMES[[i]]$moves[step])$board
    cat(i, ":", step, ":", paste(as.vector(t(board)), collapse=","), "\n", sep="")
  }
}
'''
    with tempfile.TemporaryDirectory(prefix="week05-check-") as work:
        script = Path(work) / "positions.R"
        script.write_text(r_code, encoding="utf-8")
        result = subprocess.run([args.rscript, "--vanilla", str(script), str(app)],
                                check=True, capture_output=True, text=True)
    actual = [line for line in result.stdout.splitlines() if re.match(r"\d+:\d+:", line)]
    assert actual == expected, "R board positions differ from independent chess engine"
    print(f"All {len(expected)} R board positions match the independent chess engine.")


if __name__ == "__main__":
    main()
