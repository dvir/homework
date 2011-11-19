import org.sat4j.core.VecInt;
import org.sat4j.minisat.SolverFactory;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IProblem;
import org.sat4j.specs.ISolver;
import org.sat4j.specs.TimeoutException;

/**
 * @author Amit Metodi
 */
public class SATSolver {
	private static final int INIT=0;
	private static final int SOLVED=1;
	private static final int CONFLICT=-1;

	private static final int TIMEOUT = 180;
	
	private static ISolver solver=null;
	private static int varNum;
	private static int status=SOLVED;
	private static boolean[] solution=null;
	
	public static void init(int varCnt, int timeout) {
		if(varCnt<1)
			throw new RuntimeException("SAT Solver can not be initilized with less than 1 boolean variable !");
		if(timeout<1)
			throw new RuntimeException("SAT Solver timeout must be at less 1 second !");
		SATSolver.solver = SolverFactory.newDefault();
		SATSolver.solver.setTimeout(timeout);
		SATSolver.solver.newVar(varCnt);
		SATSolver.varNum=varCnt;
		SATSolver.status=INIT;
		SATSolver.solution=null;
	}
	
	public static void init(int varCnt) {
		init(varCnt,TIMEOUT);
	}
	
	private static void verifiedClause(int[] clause) {
		if(clause==null)
			throw new RuntimeException("Can not add null clause to SAT Solver !");
		for(int i=0; i<clause.length; i++)
			if(clause[i]==0 || clause[i]>varNum || clause[i]<-varNum)
				throw new RuntimeException("Clause contains invalid number ("+clause[i]+") !");
	}
	
	public static void addClause(int[] clause) {
		switch(SATSolver.status) {
		case INIT:
			verifiedClause(clause);
			try {
				solver.addClause(new VecInt(clause));
			} catch (ContradictionException e) {
				SATSolver.status=CONFLICT;
			}
			break;
		case SOLVED:
			throw new RuntimeException("SAT Solver is not initilized !");
		}
	}
	
	public static void addClauses(int[][] clauses) {
		if(clauses==null)
			throw new RuntimeException("Can not add null clauses to SAT Solver !");
		switch(SATSolver.status) {
		case INIT:
			for(int i=0; i<clauses.length; i++)
				verifiedClause(clauses[i]);
			try {
				for(int i=0; i<clauses.length; i++)
					solver.addClause(new VecInt(clauses[i]));
			} catch (ContradictionException e) {
				SATSolver.status=CONFLICT;
			}
			break;
		case SOLVED:
			throw new RuntimeException("SAT Solver is not initilized !");
		}
	}
	
	public static boolean[] getSolution() {
		if(SATSolver.status==CONFLICT) {
			SATSolver.solution=new boolean[0];
			SATSolver.status=SOLVED;
		} else if(SATSolver.status==INIT) {
			try {
				IProblem problem = SATSolver.solver;
				if (problem.isSatisfiable()) {
					int [] model = problem.model();
					SATSolver.solution=new boolean[SATSolver.varNum+1];
					for(int i=0; i<model.length; i++)
						SATSolver.solution[Math.abs(model[i])]=(model[i]>0);
				} else {
					SATSolver.solution=new boolean[0];
				}
			} catch (TimeoutException e) {
				SATSolver.solution=null;
			} finally {
				SATSolver.status=SOLVED;
				SATSolver.solver=null;
			}
		}
		return SATSolver.solution;
	}
	
}
