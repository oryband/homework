/**
 * Represents the company's equipment repository.
 * Responsible for lending equipment for different experiments.
 *
 * @author Eldar Damari, Ory Band
 */

package company;

import java.util.Comparator;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap; 


public class Repository {

    private HashMap<String, Integer> equipment;


    public Repository(HashMap<String, Integer> equipment) {
        this.equipment = new HashMap<String, Integer>(equipment);
    }

    public Repository() {
        this.equipment = new HashMap<String, Integer>();
    }


    /**
     * @param type item's type.
     * @param additionalAmount amount of items added to repository.
     */
    public void addEquipmentToRepository(String type, int additionalAmount) {
        if (this.equipment.containsKey(type)) {
            int currentAmount = this.equipment.get(type);
            this.equipment.put(type, currentAmount + additionalAmount);
        } else {
            this.equipment.put(type, additionalAmount);
        }
    }


    public HashMap<String, Integer> getRepository() {
        return equipment;
    }


    /**
     * Iterate over each requested type, and borrow it from the repository.
     *
     * If there isn't enough of a certain type:
     *
     * 1. Return all borrowed items.
     * 2. Wait for some experiment to return its equipment.
     * 3. Try again.
     *
     * Synchronized to prevent two experiments taking the same item.
     *
     * @param requiredEquipment {type : amount}
     */
    public synchronized void acquireEquipment(
            HashMap<String, Integer> requiredEquipment) {

        boolean borrowedAllRequiredEquipment = false;
        while ( ! borrowedAllRequiredEquipment ) {

            String type, requestedType;
            Integer amount, requestedAmount;

            HashMap<String, Integer> borrowedEquipment =
                new HashMap<String, Integer>();

            boolean missingEquipment = false;

            // Sort required equipment by type (string), in order to prevent
            // deadlocks where two experiments need the same items but not in
            // the same order.
            List<String> equipmentTypes =
                new ArrayList<String>(requiredEquipment.keySet());

            Collections.sort(equipmentTypes, new Comparator<String>() {
                public int compare(String s1, String s2) {
                    return s1.compareTo(s2);
                }
            });

            // Take items from repository.
            Iterator<String> it = equipmentTypes.iterator();
            while (it.hasNext() && ! missingEquipment) {

                requestedType = it.next();
                requestedAmount = requiredEquipment.get(requestedType);

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

                // Wait for an experiment to finish,
                // becuase maybe our missing required items has been returned.
                try {
                    this.wait();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            } else {
                borrowedAllRequiredEquipment = true;
            }
        }
    }


    /**
     * Releases equipment, and returns is to repository.
     * Note function is synchronized, to prevent miscalculating returned
     * item amounts.
     *
     * @param releasedEquipment equipment hash map of returned items.
     */
    public void releaseEquipment(HashMap<String, Integer> releasedEquipment) {
        synchronized(this) {
            for (Map.Entry<String, Integer> entry : releasedEquipment.entrySet()) {
                String returnedType = entry.getKey();
                Integer returnedAmount = entry.getValue(),
                        repositoryAmount = this.equipment.get(returnedType);

                this.equipment.put(returnedType, repositoryAmount + returnedAmount);
            }

            // Wake up experiments waiting for released equipment.
            this.notifyAll();
        }
    }


    public String toString() {

        StringBuilder result = new StringBuilder();
        String N = System.getProperty("line.separator");

        result.append(N);
        result.append("Repository:" + N);
        result.append("Repository items: " + this.equipment.toString() + N);

        return result.toString();
    }
}
