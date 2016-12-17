import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import static java.util.stream.IntStream.rangeClosed;

public class NqueenJava {
    private static class Queen {
        int x;
        int y;

        Queen(int x, int y) {
            this.x = x;
            this.y = y;
        }
    }

    public static void main(String[] args) {
        NqueenJava nqueenJava = new NqueenJava();

        nqueenJava.solveNqueen(4);
    }

    private void solveNqueen(int size) {
        List<List<Queen>> solutions = placeQueens(size, size);
        System.out.println(solutions.size() + " solutions found");
    }

    private List<List<Queen>> placeQueens(int size, int n) {
        if (n == 0) {
            ArrayList<List<Queen>> solutions = new ArrayList<>();
            solutions.add(new ArrayList<>());
            return solutions;
        }
        List<List<Queen>> solutions = placeQueens(size, n - 1);
        System.out.println("n:" + n + " sols:" + solutions.size());
        List<List<Queen>> newSolutions = new ArrayList<>();

        for (List<Queen> queens : solutions) {
            rangeClosed(1, size).forEach(y -> {
                Queen queen = new Queen(n, y);
                if (isSafe(queen, queens)) {
                    queens.add(queen);
                    newSolutions.add(queens);
                }
            });
        }

        solutions.addAll(newSolutions);
        return solutions;
    }

    private boolean isSafe(Queen currentQueen, List<Queen> queens) {
        return queens.stream().allMatch(otherQueen -> !isAttacked(currentQueen, otherQueen));
    }

    private boolean isAttacked(Queen currentQueen, Queen otherQueen) {
        return currentQueen.x == otherQueen.x  // attacked in same row
                || currentQueen.y == otherQueen.y  // attacked in same column
                || Math.abs(otherQueen.x - currentQueen.x) == Math.abs(otherQueen.y - currentQueen.y); // attacked in diagonal;
    }


}
