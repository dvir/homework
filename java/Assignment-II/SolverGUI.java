import java.awt.Color;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * @author Amit Metodi
 */
public class SolverGUI extends JPanel implements ActionListener {

	private static final long serialVersionUID = 6105978439274810638L;

	private static Color[] PRINT_COLORS = { Color.blue, Color.green, Color.yellow, Color.magenta, Color.pink, Color.cyan, Color.red, Color.orange };
	private static final int BOARD_H=480, BOARD_W=560, BOARD_X=5, BOARD_Y=40;
	private int[][] board;
	private int[] inRec;
	private int cellsY,cellsX,cellSize;
	private long solveTime;
	private boolean running, stopped;
	
	private JComboBox nSizeJCB,mSizeJCB,colorsJCB,solverJCB;
	private JButton solveBTN, cancelBTN;
	
	Thread solvingThread;
	
	public SolverGUI() {
		this.setBackground(Color.white);
		this.board=null;
		this.running=false;
		this.stopped=false;
		this.solvingThread=null;

		this.board=new int[][] {
				{0,1,2,3,4,5,6,7,0},
				{1,2,3,4,5,6,7,0,1},
				{2,3,4,5,6,7,0,1,2},
				{3,4,5,6,7,0,1,2,3},
				{4,5,6,7,0,1,2,3,4},
				{5,6,7,0,1,2,3,4,5},
				{6,7,0,1,2,3,4,5,6},
				{7,0,1,2,3,4,5,6,7},
				{0,1,2,3,4,5,6,7,0}
				};
		this.inRec=new int[] { 0,0, 8,8 };
		this.solveTime=1234567890;
		this.calcBoardSize();
		this.setGUI();
	}
	
	private void setGUI() {
		this.setLayout(null);
		String[] nORm = new String[19];
		for(int i=2; i<=20; i++)
			nORm[i-2]=""+i;

		JLabel label1=new JLabel("board");
		label1.setSize(40, 25);
		label1.setLocation(5,5);
		this.add(label1);
		
		this.nSizeJCB = new JComboBox(nORm);
		this.nSizeJCB.setLocation(45, 5);
		this.nSizeJCB.setSize(40,25);
		this.nSizeJCB.setSelectedIndex(2);
		this.add(this.nSizeJCB);

		label1=new JLabel("x");
		label1.setSize(10, 25);
		label1.setLocation(90,5);
		this.add(label1);
		
		this.mSizeJCB = new JComboBox(nORm);
		this.mSizeJCB.setLocation(105, 5);
		this.mSizeJCB.setSize(40,25);
		this.mSizeJCB.setSelectedIndex(2);
		this.add(this.mSizeJCB);
		
		String[] clrs = new String[7];
		for(int i=2; i<=8; i++)
			clrs[i-2]=""+i;
		
		label1=new JLabel("colors");
		label1.setSize(40, 25);
		label1.setLocation(160,5);
		this.add(label1);

		this.colorsJCB = new JComboBox(clrs);
		this.colorsJCB.setLocation(200, 5);
		this.colorsJCB.setSize(40,25);
		this.colorsJCB.setSelectedIndex(1);
		this.add(this.colorsJCB);
		
		label1=new JLabel("solver");
		label1.setSize(40, 25);
		label1.setLocation(255,5);
		this.add(label1);

		this.solverJCB = new JComboBox(new String[] { "Basic", "Random" , "SAT based" });
		this.solverJCB.setLocation(300, 5);
		this.solverJCB.setSize(100,25);
		this.solverJCB.setSelectedIndex(0);
		this.add(this.solverJCB);
		
		this.solveBTN = new JButton("Find Solution");
		this.solveBTN.setLocation(410, 5);
		this.solveBTN.setSize(160,25);
		this.solveBTN.addActionListener(this);
		this.add(this.solveBTN);

		this.cancelBTN = new JButton("Please wait while searching for a solution... Click here to stop the search.");
		this.cancelBTN.setLocation(5, 40);
		this.cancelBTN.setSize(565,40);
		this.cancelBTN.addActionListener(this);
		this.cancelBTN.setVisible(false);
		this.add(this.cancelBTN);
	}
	
	
	private void calcBoardSize() {
		if(this.board==null) return;
		this.cellsY=this.board.length;
		this.cellsX=this.board[0].length;
		this.cellSize = Math.min(BOARD_H / this.cellsY, BOARD_W / this.cellsX);
	}
	
	public void paintComponent(Graphics grph) {
		super.paintComponent(grph);
		if(!this.running) {
			if(this.board != null) {
				for(int y=0; y<this.cellsY; y++)
					for(int x=0; x<this.cellsX; x++) {
						grph.setColor(PRINT_COLORS[this.board[y][x]]);
						grph.fillRect(x*this.cellSize+BOARD_X, y*this.cellSize+BOARD_Y, this.cellSize, this.cellSize);
						grph.setColor(Color.black);
						grph.drawRect(x*this.cellSize+BOARD_X, y*this.cellSize+BOARD_Y, this.cellSize, this.cellSize);
					}
				if(this.inRec!=null) {
					final int y1 = (int)((this.inRec[0]+0.25)*this.cellSize+BOARD_Y);
					final int x1 = (int)((this.inRec[1]+0.25)*this.cellSize+BOARD_X);
					final int y2 = (int)((this.inRec[2]+0.25)*this.cellSize+BOARD_Y);
					final int x2 = (int)((this.inRec[3]+0.25)*this.cellSize+BOARD_X);
					final int s = this.cellSize/2;
					grph.setColor(Color.white);
					grph.fillOval(x1,y1,s,s);
					grph.fillOval(x1,y2,s,s);
					grph.fillOval(x2,y1,s,s);
					grph.fillOval(x2,y2,s,s);

					grph.setColor(Color.black);
					grph.drawOval(x1,y1,s,s);
					grph.drawOval(x1,y2,s,s);
					grph.drawOval(x2,y1,s,s);
					grph.drawOval(x2,y2,s,s);

					final int y=(int)((this.inRec[0]+0.5)*this.cellSize+BOARD_Y);
					final int x=(int)((this.inRec[1]+0.5)*this.cellSize+BOARD_X);
					final int h=(this.inRec[2]-this.inRec[0])*this.cellSize;
					final int w=(this.inRec[3]-this.inRec[1])*this.cellSize;

					grph.drawRect(x,y,w,h);

					grph.setColor(Color.red);
					grph.drawString("Invalid solution found in "+this.solveTime+" ms.", BOARD_X, BOARD_Y+this.cellsY*this.cellSize+20);

				} else {
					grph.setColor(Color.black);
					grph.drawString("Valid solution found in "+this.solveTime+" ms.", BOARD_X, BOARD_Y+this.cellsY*this.cellSize+20);
				}
			} else if(stopped) {
				grph.setColor(Color.red);
				grph.drawString("Stopped after "+this.solveTime+" ms.", BOARD_X, BOARD_Y+20);
			} else {
				grph.setColor(Color.red);
				grph.drawString("No solution in "+this.solveTime+" ms.", BOARD_X, BOARD_Y+20);
			}
		}
	}
	
	private void setInputEnable(boolean aFlag) {
		this.nSizeJCB.setEnabled(aFlag);
		this.mSizeJCB.setEnabled(aFlag);
		this.colorsJCB.setEnabled(aFlag);
		this.solverJCB.setEnabled(aFlag);
		this.solveBTN.setEnabled(aFlag);
		this.cancelBTN.setVisible(!aFlag);
	}
	
	private void startRunning() {
		this.running=true;
		this.stopped=false;
		this.setInputEnable(false);
		this.repaint();
		final int n=this.nSizeJCB.getSelectedIndex() + 2;
		final int m=this.mSizeJCB.getSelectedIndex() + 2;
		final int c=this.colorsJCB.getSelectedIndex() + 2;
		final int s=this.solverJCB.getSelectedIndex();
		this.solvingThread=new Thread() {
			public void run() {
				solveTime = System.currentTimeMillis();
				switch(s) {
				case 0:
					board=Part1.solver(n, m, c);
					break;
				case 1:
					board=Part1.randomSolver(n, m, c, n*m*c,n*m*c);
					break;
				case 2:
					board=Part2.satBasedSolver(n, m, c);
					break;
				default:
					board=null;
					break;
				}
				solveTime = System.currentTimeMillis() - solveTime;
				if(board!=null) {
					inRec=Part1.findSameColorRec(board);
					calcBoardSize();
				}
				running=false;
				setInputEnable(true);
				solvingThread=null;
				repaint();
			}
		};
		this.solvingThread.start();
	}
	
	
	@Override
	public void actionPerformed(ActionEvent aevnt) {
		if(aevnt.getSource()==this.solveBTN) {
			this.startRunning();
		} else if(aevnt.getSource()==this.cancelBTN) {
			if(this.solvingThread!=null) {
				this.solvingThread.stop();
				this.solvingThread=null;
				this.board=null;
				this.solveTime = System.currentTimeMillis() - this.solveTime;
				this.stopped=true;
				this.running=false;
				this.setInputEnable(true);
				this.repaint();
			}
		}
	}
	
	public static void main(String[] args) {
		JFrame frame=new JFrame("Introduction to Computer Science 2012 - Assignment 2");
		frame.setSize(600, 600);
		frame.add(new SolverGUI());
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.setVisible(true);
	}
}
