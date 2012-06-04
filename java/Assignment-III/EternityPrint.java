import java.awt.Color;
import java.awt.Graphics;
import java.awt.Polygon;
import javax.swing.JComponent;
import javax.swing.JFrame;

public class EternityPrint extends JComponent {
	private static final long serialVersionUID = 4347521825761027221L;

	public static final Color[] COLORS = { Color.gray, Color.red, Color.green,
			Color.blue, Color.orange, Color.white, Color.yellow, Color.pink,
			Color.magenta, Color.black // error color
	};

	public static final int PIECE_SIZE = 50;

	private Polygon[] polys;
	private int[] clrsIndx;

	protected EternityPrint(int[][][] board) {
		this.createPolygons(board);
	}

	private int findLongestRow(int[][][] board) {
		int res = 0;
		for (int i = 0; i < board.length; i++)
			if (board[i] != null && board[i].length > res)
				res = board[i].length;
		return res;
	}

	private void createPolygons(int[][][] board) {
		if (board != null) {
			final int PIECES = 4;
			final int MAXROW = this.findLongestRow(board);
			this.polys = new Polygon[board.length * MAXROW * PIECES];
			this.clrsIndx = new int[board.length * MAXROW * PIECES];
			for (int i = 0; i < board.length; i++)
				if (board[i] != null)
					for (int j = 0; j < board[i].length; j++)
						if (board[i][j] != null && board[i][j].length == PIECES)
							for (int p = 0; p < PIECES; p++) {
								final int PINDX = p + j * PIECES + i * MAXROW * PIECES;
								this.polys[PINDX] = this.createPiece(i, j, p);
								this.clrsIndx[PINDX] = board[i][j][p];
								if (this.clrsIndx[PINDX] >= COLORS.length - 1)
									this.clrsIndx[PINDX] = COLORS.length - 1;
							}
		} else {
			this.polys = new Polygon[0];
			this.clrsIndx = new int[0];
		}
	}

	private Polygon createPiece(int i, int j, int p) {
		Polygon po = new Polygon();
		int left = i * PIECE_SIZE;
		int top = j * PIECE_SIZE;
		int midx = left + PIECE_SIZE / 2;
		int midy = top + PIECE_SIZE / 2;
		po.addPoint(midx, midy);
		switch (p) {
			case 0:
				po.addPoint(left, top);
				po.addPoint(left + PIECE_SIZE, top);
				break;
			case 1:
				po.addPoint(left + PIECE_SIZE, top);
				po.addPoint(left + PIECE_SIZE, top + PIECE_SIZE);
				break;
			case 2:
				po.addPoint(left, top + PIECE_SIZE);
				po.addPoint(left + PIECE_SIZE, top + PIECE_SIZE);
				break;
			case 3:
				po.addPoint(left, top);
				po.addPoint(left, top + PIECE_SIZE);
				break;
		}

		return po;
	}

	@Override
	public void paint(Graphics g) {
		for (int i = 0; i < this.polys.length; i++)
			if (this.polys[i] != null) {
				g.setColor(COLORS[this.clrsIndx[i]]);
				g.fillPolygon(this.polys[i]);
				g.setColor(Color.black);
				g.drawPolygon(this.polys[i]);
			}
	}

	public static void showBoard(int[][][] board) {
		JFrame window = new JFrame();
		window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		window.setBounds(30, 30, 300, 300);
		window.getContentPane().add(new EternityPrint(board));
		window.setVisible(true);
	}
}
