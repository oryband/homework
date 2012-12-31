/**
 * Represents a company's equipment repository.
 *
 * @author Eldar Damari, Ory Band
 */
//package company;


import java.io.*;
import java.util.Iterator;
import java.util.Map;
import java.util.HashMap; 


/**
 * Responsible for lending equipment for different experiments.
 */
public class Repository {

    private HashMap<String, Integer> equipment;


    public Repository(HashMap<String, Integer> equipment) {
        this.equipment = new HashMap<String, Integer>(equipment);
    }

    // TODO Do we really need to init empty member?
    public Repository() {
        this.equipment = new HashMap<String, Integer>();
    }


    /**
     * @param type added item type.
     * @param additionalAmount amount of items added to repository.
     */
    public void addItemToRepository(String type, int additionalAmount) {
        int currentAmount = this.equipment.get(type);
        this.equipment.put(type, currentAmount + additionalAmount);
    }

    // TODO Delete this. Used in util for initialization Repository. This should be done in the constructor.
    public HashMap<String, Integer> getRepository() {
        return equipment;
    }

    // TODO
    /**
     * @param requiredEquipment {type : amount}
     */
    public synchronized void aquireEquipment(HashMap<String, Integer> requiredEquipment) {
        boolean borrowedAllRequiredEquipment = false;


        // Iterate over each requested type, and borrow it from the repository.
        // If there isn't enough of a certain type, return all borrowed items
        // and wait for some experiment to return its equipment,
        // and try again.
        while ( ! borrowedAllRequiredEquipment ) {

            String type, requestedType;
            Integer amount, requestedAmount;
            HashMap<String, Integer> borrowedEquipment =
                new HashMap<String, Integer>();
            boolean missingEquipment = false;
            Iterator it = requiredEquipment.entrySet().iterator();

            while (it.hasNext() && ! missingEquipment) {
                // TODO Warning because of unchecked cast.
                Map.Entry<String, Integer> entry =
                    (Map.Entry<String, Integer>) it.next();

                requestedType = entry.getKey();
                requestedAmount = entry.getValue();

                amount = this.equipment.get(requestedType);
                if (amount >= requestedAmount) {
                    this.equipment.put(requestedType, amount - requestedAmount);
                    borrowedEquipment.put(requestedType, amount);
                } else {
                    missingEquipment = true;
                }
            }

            if (missingEquipment) {
                // Return all borrowed items to repository.
                for (Map.Entry<String, Integer> entry :
                        borrowedEquipment.entrySet()) {

                    type = entry.getKey();
                    amount = entry.getValue();  // Borrowed amount.

                    // Amount in repository.
                    int amountInRepository = this.equipment.get(type);

                    // Return borrowed amount back to repository.
                    this.equipment.put(type, amountInRepository + amount);
                }
            } else {
                borrowedAllRequiredEquipment = true;
            }

            try {
                this.wait();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }


    public String toString() {

        StringBuilder result = new StringBuilder();
        String NEW_LINE = System.getProperty("line.separator");
        
        result.append("______________________________________" + NEW_LINE);
        result.append("           ---Repository---: " + NEW_LINE);
        result.append("EquipmentPackage data: " + NEW_LINE);
        result.append(this.equipment.toString() + NEW_LINE);
        return result.toString();
    }
}
