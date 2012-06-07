import java.util.* ;
import java.io.* ;
public class Main {

   public static void main(String[] args){
	   String input1FileName = args[0];
	   String input2FileName = args[1];
 	   String outputFileName = args[2];
	   
	   String str; // the data from the file
	   String[] ids; // array of ids read from file

	   /** First section - Registered Passengers List **/
 	   str = read_From_File_Example(input1FileName); // read the file to a string
 	   ids = str.split(","); // now you have an array of ids as strings (but they contain spaces)
 	   int N = ids.length; // the amount of passengers we can board on the plane (amount of registered passengers)
 	   
 	   HashTable registeredPassengers = new HashTable(N); // define the registered passengers ids hash table
 	    
 	   for(int i = 0; i < N ; i++) {
 		   ids[i] = ids[i].trim(); //remove spaces
	    	
 		   // add the id to the registered passengers hash table,
 		   // to allow quick search on the list.
 		   registeredPassengers.insert(Integer.parseInt(ids[i]));
	   }
 	     
 	   write_To_File_Example(outputFileName, registeredPassengers.toString()); // write N to the output file
 	    
 	   /** Second section - Check In List **/
 	   str = read_From_File_Example(input2FileName); // read the file to a string
 	   ids = str.split(","); // now you have an array of ids as strings (but they contain spaces
 	   int passengersArrivedCount = ids.length; // this is N - the amount of ids
 	   
 	   // initialize the boarded passengers array with the size of the registered passengers count.
 	   // we insert ids of passengers to the top of the array if they are registered for the flight,
 	   // and insert from the bottom of the array if they are not registered for it.
 	   // later, we will reverse the order of the -NOT- registered people in the array, in order to have an ordered list
 	   // of the passengers we shall board for the flight.
 	   int[] boardedPassengers = new int[N];
	   int lastRegisteredIndex = -1;
	   int firstNotRegisteredIndex = N;
 	   
	   int stepsAmount = 0;
	   
	   int id;
	   int searchResult; // may hold a positive number steps took to find the id, 
	   					 // or a negative number with an absolute value of steps took we searched when we didn't find the id.
 	   for(int i = 0; i < passengersArrivedCount; i++) {
 		   ids[i] = ids[i].trim(); //remove spaces
 		   id = Integer.parseInt(ids[i]);
 		   
 		   // add the id to the boarded passengers list
 		   // if the id is registered, add to the top and increase the respective counter.
 		   // if he's not registered, add it from the bottom and decrease the respective counter.
 		   // NOTE: if the registered index passed the not registered index, stop adding non-registered passengers, and start overwriting not-registered ones.
 		   searchResult = registeredPassengers.search(id);
 		   stepsAmount += Math.abs(searchResult);
 		   
 		   if (searchResult > 0) {
 			   lastRegisteredIndex++;
 			   boardedPassengers[lastRegisteredIndex] = id;
 		   }
 		   else if (firstNotRegisteredIndex-1 > lastRegisteredIndex) {  		   
 			   firstNotRegisteredIndex--;
 			   boardedPassengers[firstNotRegisteredIndex] = id; 			   
 		   } 			   
	   }
 	     
 	   // we have to update the firstNotRegisteredIndex to be the max out of the firstNotRegisteredIndex and lastRegisteredIndex, 
 	   // in case we overwritten some of the not registered people.
 	   // NOTE: if firstNotRegisteredIndex is larger than the amount of people we can board on the plane,
 	   // then we are not gonna allow any of the not registered people to board.
 	   firstNotRegisteredIndex = Math.max(firstNotRegisteredIndex, lastRegisteredIndex+1);
 	   
 	   // we have to reverse the order of the not registered people on the boardedPassengers list - this will be our check-in ordered list
	   // we switch the passengers at firstNotRegisteredIndex and N, and then firstNotRegisteredIndex+1 and N-1, and so on...
 	   int tmp;
 	   for (int i = 0; i < (N-firstNotRegisteredIndex)/2; i++) {
 		   tmp = boardedPassengers[firstNotRegisteredIndex+i];
 		   boardedPassengers[firstNotRegisteredIndex+i] = boardedPassengers[N-i-1];
 		   boardedPassengers[N-i-1] = tmp;
 	   }
 	   
 	   write_To_File_Example(outputFileName, "" + (stepsAmount / passengersArrivedCount)); // write average of steps per search to the output file
 	    
 	   int[] sortedBoardedPassengers = new int[N]; // holds the sorted boarded passengers list, by id, from smallest to largest
 	   // copy the boarded passengers list to the new array
 	   for (int i = 0; i < N; i++) {
 		   sortedBoardedPassengers[i] = boardedPassengers[i];
 	   }
 	   
 	   // sort the new list - bubble sort
 	   boolean swapped = true;
 	   for (int i = 0; i < N && swapped; i++) {
 		   swapped = false;
 		   for (int j = 1; j < N; j++) {
 			   if (sortedBoardedPassengers[j-1] > sortedBoardedPassengers[j]) {
 		 		   tmp = sortedBoardedPassengers[j];
 		 		   sortedBoardedPassengers[j] = sortedBoardedPassengers[j-1];
 		 		   sortedBoardedPassengers[j-1] = tmp;
 		 		   swapped = true;
 			   }
 		   }
 	   }
 	   
 	   String idsList = "";
 	   if (sortedBoardedPassengers.length > 0) {
 		   idsList = "" + sortedBoardedPassengers[0];
 	 	   for (int i = 1; i < sortedBoardedPassengers.length; i++) {
 	 		   idsList += ", " + sortedBoardedPassengers[i];
 	 	   } 		   
 	   }
 	   
 	   write_To_File_Example(outputFileName, idsList); // write the ids of the sorted boarded passengers list.
 	  boolean useFirstHash = true;
 	  
 	  for (int j = 0; j < 2; j++) {
 		  int steps;
	 	  int count = 0;
	 	  int countFirstHalfSteps = -1;
	 	  int countFirstThreeQuartersSteps = -1;
	 	  int countFirstNMinusSqrtNSteps = -1;
	 	  int countLastSqrtNSteps = -1;
	 	  CheckInTable airplane = new CheckInTable(N);
	 	  for (int i = 0; i < N; i++) {
	 		 steps = airplane.checkIn(boardedPassengers[i], useFirstHash);
	 		 count += steps;
	 		 //write_To_File_Example(outputFileName, (useFirstHash ? "[1] " : "[2] ") + i + " did " + steps + ". Total: " + count);
	 		 
	 		 if (countFirstHalfSteps == -1 && i+1 >= N / 2) {
	 			countFirstHalfSteps = count;
	 		 }
	 		 if (countFirstThreeQuartersSteps == -1 && i+1 >= 3*N/4) {
	 			countFirstThreeQuartersSteps = count;
	 		 }
	 		 if (countFirstNMinusSqrtNSteps == -1 && i+1 >= N-Math.sqrt(N)) {
	 			countFirstNMinusSqrtNSteps = count;
	 		 } 
	 	  }
	 	  
	 	 countLastSqrtNSteps = count - countFirstNMinusSqrtNSteps; // N - (N - sqrt(N)) = sqrt(N)
	 	  
	 	  write_To_File_Example(outputFileName, countFirstHalfSteps + "," + countFirstThreeQuartersSteps + "," + countFirstNMinusSqrtNSteps + "," + countLastSqrtNSteps);
	 	  useFirstHash = !useFirstHash;
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