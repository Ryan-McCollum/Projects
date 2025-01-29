/////////////////////////////////////////////////////////////////
//
// Project 4
// author: Ryan McCollum
// date: 22 Nov 2022
// CSE 174
//
// PROGRAM: 
//    This program reads from a file of shipping orders and 
//       converts it into an ArrayList.
//    Each order has a transport ID, shipping ID, 
//       reciever name and tracking number.
//    The program will sort the loaded ArrayList by either
//       the shipping ID or tracking number.
//    The user can also choose to display 10 Shipping objects
//       at a time of either the sorted or unsorted ArrayList.
//


// importing
import java.util.Scanner;
import java.util.InputMismatchException;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;

public class Project4 {
    
    // Establishing global scanner
    static Scanner input = new Scanner(System.in);

    public static void main(String[] args) throws FileNotFoundException {
        // Establishing important variables
        int option;
        boolean option1 = false;
        boolean optionSorted = false;
        ArrayList<Shipping> shipment = new ArrayList<Shipping>();
        ArrayList<Shipping> shipmentClone = new ArrayList<Shipping>();


        // Running program until user enters 6
        do {
            
            // Displaying menu and getting user decision
            menu();
            option = input.nextInt();
            switch (option) {
                
                // Loads file and creates ArrayList of Shipping objects
                case 1:
                    shipment = loadFile();
                    shipmentClone = clone(shipment);
                    option1 = true;
                    optionSorted = false;
                    break;
                    
                // Displays unsorted list
                case 2:
                    display(shipmentClone, option1);
                    break;
                    
                // Sorts by shippingID
                case 3:
                    sort1(shipment, option1);
                    optionSorted = true;
                    break;
                    
                // Sorts by tracking number
                case 4:
                    sort2(shipment, option1);
                    optionSorted = true;
                    break;
                    
                // Displays sorted list
                case 5:
                    if (option1 == true) {
                        display2(shipment, optionSorted);
                    } else {
                        display(shipment, option1);
                    }
                    break;
                    
                    
                // Ends program
                case 6:
                    System.out.printf("End!");
                    break;
                    
                default:
                    System.out.println("Invalid input");
                    break;
            }
        } while (option != 6);
    }
    
    
    
    /*
    * Prints a menu with options on the display.
    */
    private static void menu() {
        System.out.printf("1. Load from a file\n");
        System.out.printf("2. Print from the loaded list\n");
        System.out.printf("3. Sort the list based on shipment IDs\n");
        System.out.printf("4. Sort the list based on the tracking numbers\n");
        System.out.printf("5. Print the sorted list\n");
        System.out.printf("6. Exit\nEnter a number [1-6]: \n");
    }
    
    /**
    * Loads file and creates an ArrayList of shipping objects.
    * returns an ArrayList of shipping objects
    */
    public static ArrayList<Shipping> loadFile() throws FileNotFoundException {
        ArrayList<Shipping> loadedFile = new ArrayList<Shipping>();
        String fileName = "";

        try {
            System.out.println("Enter the name of the file: ");
            fileName = input.next();
            Scanner fileReader = new Scanner(new File(fileName));
            while (fileReader.hasNextInt()) {
                int transId = fileReader.nextInt();
                int shipId = fileReader.nextInt();
                String recName = fileReader.next();
                String trackName  = fileReader.next();
                Shipping a = new Shipping(transId, shipId, recName, trackName);
                loadedFile.add(a);    
            }
        } catch (Exception e) {
            System.out.println("");
        }
        System.out.printf("Loading from the file is done!\n\n");
        return loadedFile;
    }

    /**
    * Displays 10 Shipping objects at a time of the given ArrayList 
    * if data has been loaded.
    * @param shipList is the ArrayList of Shipping objects being displayed
    * @param test makes sure that data has been loaded
    */ 
    public static void display(ArrayList<Shipping> shipList, boolean test) {
        
        // Testing to make sure data has been loaded
        if (test == false) {
            System.out.println("No data has been loaded yet!");
        } else {
            System.out.println("**** Printing the list ****");
            String ask = "";
            
            // Establishing while loop that displays 10 objects every time
            int j = 10;
            int k = 0; 
            while (!ask.equals("s")) {
                // runs 10 times, starting with last shipment not printed
                for (int i = k; i < j; i++) {
                    System.out.println(shipList.get(i));             
                }
                
                j = j + 10;
                k = k + 10;
                // Asks user if they would like to continue
                System.out.printf("Enter something to continue");
                System.out.printf("/enter s to stop\n");
                ask = input.next();
                // if j exceeds size of list then stop, nothing more to print
                if (j >= shipList.size()) {
                    ask = "s";
                }
            }
            System.out.println("");
        }
    }
    
    /**
    * Displays 10 Shipping objects at a time of the given ArrayList 
    * if data has been loaded and stored.
    * @param shipList is the ArrayList of Shipping objects being displayed
    * @param test makes sure that data has been sorted
    */
    public static void display2(ArrayList<Shipping> shipList, boolean test) {
     
        // Testing to make sure data has been sorted
        if (test == false) {
            System.out.println("Nothing sorted yet!");
        } else {
            System.out.println("**** Printing the list ****");
            String ask = "";
            
            // Establishing while loop that displays 10 objects every time
            int j = 10;
            int k = 0; 
            while (!ask.equals("s")) {
                // runs 10 times, starting with last shipment not printed
                for (int i = k; i < j; i++) {
                    System.out.println(shipList.get(i));             
                }
                
                j = j + 10;
                k = k + 10;
                // Asks user if they would like to continue
                System.out.printf("Enter something to continue");
                System.out.printf("/enter s to stop\n");
                ask = input.next();
                // if j exceeds size of list then stop, nothing more to print
                if (j >= shipList.size()) {
                    ask = "s";
                }
            }
            System.out.println("");
        }
    }
    
    /**
    * Sorts the given ArrayList by shipping IDs.
    * @param list is the ArrayList of Shipping objects being sorted
    * @param test makes sure that data has been loaded
    */    
    public static void sort1(ArrayList<Shipping> list, boolean test) {
        
        // Testing to see if data has loaded
        if (test == false) {
            System.out.println("No data has been loaded yet!");
        } else {
        
            // Sorting data by shipping ID
            boolean swap;
            do {
                swap = false; 
                for (int i = 0; i < list.size() - 1; i++) {
                    // compare elements that are next to each other
                    if (list.get(i).getShipmentID() > list.get(i + 1).getShipmentID()) {
                        Shipping temp = list.get(i);
                        list.set(i, list.get(i + 1));
                        list.set(i + 1, temp);
                        swap = true;
                    }
                }  
            } while (swap);
            System.out.printf("Sorting is done!\n\n");
        } 
    } 
    
    /**
    * Sorts the given ArrayList by tracking numbers.
    * @param list is the ArrayList of Shipping objects being sorted
    * @param test makes sure that data has been loaded
    */  
    public static void sort2(ArrayList<Shipping> list, boolean test) {
        
        // Testing to see if data has been loaded
        if (test == false) {
            System.out.println("No data has been loaded yet!");
        } else {
            
            // Sorting ArrayList by tracking number
            boolean swap;
            do {
                swap = false; 
                for (int i = 0; i < list.size() - 1; i++) {
                    // compare elements that are next to each other
                    if (list.get(i).getTrackingNumber().compareTo(list.get(i + 1).getTrackingNumber()) > 0) {
                        Shipping temp = list.get(i);
                        list.set(i, list.get(i + 1));
                        list.set(i + 1, temp);
                        swap = true;
                    }
                }  
            } while (swap);
            System.out.printf("Sorting is done!\n\n");
        } 
    } 
    
    /**
    * Copys the given ArrayList so that there can be a sorted 
    * and unsorted version.
    * @param list is the ArrayList of Shipping objects being copied
    */  
    public static ArrayList<Shipping> clone(ArrayList<Shipping> arr) {
    
        // Establishing new ArrayList and copying the first ArrayList into it
        ArrayList<Shipping> newList = new ArrayList<Shipping>();
        for (int i = 0; i < arr.size(); i++) {
            newList.add(arr.get(i));
        }
        
        // return new copied list
        return newList;
    }
}