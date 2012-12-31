/**
 * Represents a Science Store.
 *
 * @author Eldar Damari, Ory Band
 */
//package company;


import java.util.Comparator;

import java.util.Collections;
import java.util.ListIterator;
import java.util.Map;
import java.util.HashMap; 
import java.util.ArrayList; 


/**
 * Responsible for purchasing equipment, scientists and laboratories.
 */
public class ScienceStore implements ScienceStoreInterface {  
 
    private HashMap<String, ArrayList<EquipmentPackage>> equipmentPackages;
    private HashMap<String, ArrayList<Scientist>> scientists;
    private HashMap<String, ArrayList<Laboratory>> laboratories;


    /**
     * Besides initializing members,
     * sorts packages by amount, from largest to smallest,
     * and sorts scientists by price, from most expensive to cheapest.
     */
    public ScienceStore( 
            HashMap<String, ArrayList<EquipmentPackage>> equipmentPackages,
            HashMap<String, ArrayList<Scientist>> scientists,
            HashMap<String, ArrayList<Laboratory>> laboratories) {

        // FIXME Produces warnings since arguments are unchecked.
        this.equipmentPackages = new HashMap(equipmentPackages);
        this.scientists = new HashMap(scientists);
        this.laboratories = new HashMap(laboratories);

        // Sort equipment (amount) from largest to smallest,
        // and sorts scientists from cheapest to most expensive.
        for (Map.Entry<String, ArrayList<EquipmentPackage>> entry
                : this.equipmentPackages.entrySet()) {

            Collections.sort(entry.getValue());
            Collections.reverse(entry.getValue());
        }

        for (Map.Entry<String, ArrayList<Scientist>> entry :
                this.scientists.entrySet()) {

            Collections.sort(entry.getValue());
        }
    }


    public void purchaseEquipmentPackages (
            Repository repository, Statistics statistics, Map<String, Integer> requestedEquipment) {

        // Iterate over each request equipment type,
        // and search for the closest matching package size that overexceeds 
        // its size.
        String requestedType;
        Integer requestedAmount;
        int price,
            amount;
        boolean found,
                tooSmall;
        EquipmentPackage equipmentPackage;
        for (Map.Entry<String, Integer> requestedEntry :
                requestedEquipment.entrySet()) {

            requestedType = requestedEntry.getKey();
            requestedAmount = requestedEntry.getValue();

            ListIterator<EquipmentPackage> it =
                this.equipmentPackages.get(requestedType).listIterator();

            // Search for the closests matching overexceeding package size.

            found = false;
            tooSmall = false;
            while (it.hasNext() && ! tooSmall ) {
                equipmentPackage = it.next();
                if (equipmentPackage.getAmount() >= requestedAmount) {
                    found = true;
                } else {
                    tooSmall = true;
                }
            }

            if ( ! found ) {
                System.out.println(
                        "Science Store: No matching package size for requested item '"
                        + requestedType + "'.");
            } else {
                amount = equipmentPackage.getAmount();
                price = equipmentPackage.getPrice();
                statistics.chargePrice(price);
                equipmentPackage.decrementAmount();
                repository.addItemToRepository(requestedType, amount);
            }
        }
    }

    public boolean purchaseScientist (
            Statistics statistics, Map<String, Integer> requestedScientists) {}

    public boolean purchaseLaboratory (
            Statistics statistics, String requestedLaboratory) {}


    private EquipmentPackage calculateBestPackage(
            String requestedEquipment, int requestedAmount) {}
    private Scientist calculateBestScientist(String requestedSpecialization) {}
    private Laboratory calculateBestLaboratory(String requestedSpecialization) {}

    
    // TODO Do we really need getters?
    // Getters
    public HashMap<String, ArrayList<EquipmentPackage>> getEquipmentPackages() {
        return this.equipmentPackages;
    }

    public HashMap<String, ArrayList<Scientist>> getScientists() {
        return this.scientists;
    }

    public HashMap<String, ArrayList<Laboratory>> getLaboratories() {
        return this.laboratories;
    }
    // TODO Do we really need these?
    // For tests -- TODO huh ?
    public boolean isEquipmentPackageEmpty(EquipmentPackage EquipmentPackage) {}
    public boolean isScientistsEmpty(String specialization) {}
    public boolean isLaboratoriesEmpty(String specialization) {}
}
