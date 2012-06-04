import java.util.* ;
import java.io.* ;
public class Main {

   public static void main(String[] args){
   	
 	    String str = read_From_File_Example("input1.dat");//read the file to a string
 	    String[] ids = str.split(",");//now you have an array of ids as strings (but they contain spaces
 	    int N = ids.length;//this is N - the amount of ids
 	   
 	    for(int i = 0; i <N ;i++)
	    {
	    	ids[i] = ids[i].trim();//remove spaces
	    }
 	    write_To_File_Example("output1.dat","number of ids = " + N);//write N to the output file
 	    for(int i = 0; i <N ;i++)
	    {
 	    	write_To_File_Example("output1.dat","id number " + i + " : " + ids[i]);//write ids[i] to the output file
	    }
 	   
   }
   
   
   public static String read_From_File_Example(String inputFilename){
	    String idsString = "" ;
        try {
            File inFile = new File(inputFilename);
            FileReader ifr = new FileReader(inFile);
            BufferedReader ibr= new BufferedReader(ifr) ;

            String tempLine = "" ;
           
            while (tempLine != null )
            {
            	tempLine = ibr.readLine() ;
                if (tempLine != null)
                {
                	idsString += tempLine;
                }
            }
            
            ibr.close();
            ifr.close();
            
        }

        catch(Exception e) {
            System.out.println( "Error \""+ e.toString()+ "\" on file "+inputFilename);
            e.printStackTrace() ;
            System.exit(-1) ;      //brutally exit the program
        }
        return idsString;

    }


    private static void write_To_File_Example(String outputFilename,String str) {

        
        try {
            File outFile = new File(outputFilename);
            FileWriter ofw = new FileWriter(outFile, true);

            
          	ofw.append(str+"\r\n");

            ofw.close();
        }
        catch (Exception e) {
            System.out.println( "Error \""+ e.toString()+ "\" on file "+outputFilename);
            e.printStackTrace() ;
            System.exit(-1) ;      //brutally exit the program
        }

    }
    
  
}