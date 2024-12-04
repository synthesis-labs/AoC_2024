package garvie.Java.DayOne;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.io.FileNotFoundException;  // Import this class to handle errors
import java.io.IOException;
import java.util.List;

class ReadFile {
    static Object[] read()
        throws IOException  {
        Integer[] firstColumn = null;
        Integer[] secondColumn = null;
    try {
        List<String> inputStrings = Files.readAllLines(Paths.get("/Users/michaelgarvie/Documents/Work Documentation/2024/AdventOfCode/AoC_2024/garvie/Java/DayOne/test_input.txt"));
        String[] inputArray = inputStrings.toArray(new String[0]);
        Integer lengthOfArray = inputArray.length;
        firstColumn = new Integer[lengthOfArray];
        secondColumn = new Integer[lengthOfArray];

        for (int i = 0; i < lengthOfArray; i++) {
            int lengthOfString = inputArray[i].length();
            firstColumn[i] = inputArray[i].charAt(0) - '0';
            secondColumn[i] = inputArray[i].charAt(lengthOfString-1) - '0';
        }

    } catch (FileNotFoundException e) {
        System.out.println("An error occurred");
    } 

    Object[] outputArrays = new Object[2];
    outputArrays[0] = firstColumn;
    outputArrays[1] = secondColumn;
    return outputArrays;
    }
}

class OrderColumn {
    static Integer[] sort(Integer[] column) {

        Integer lengthOfColumn = column.length;

        Integer[] outputColumn = null;
        return outputColumn;
    }
}

class DayOne { 
    public static void main (String[] args) {
        Object[] OriginalColumns = null;
        try {
        OriginalColumns = ReadFile.read();
        } catch (IOException e) {
            System.out.println("An IOException error occurred");
        }
        Integer[] leftColumn = (Integer[]) OriginalColumns[0];
        Integer[] rightColumn = (Integer[]) OriginalColumns[1];
        
        Integer[] outputLeftColumn = OrderColumn.sort(leftColumn);
        Integer[] outputRightColumn = OrderColumn.sort(rightColumn);
        
    }
}