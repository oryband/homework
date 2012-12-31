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
            Repository repository,
            Statistics statistics,
            Map<String, Integer> requestedEquipment) {

        // Iterate over each requested equipment type,
        // and search for the closest matching package size that overexceeds 
        // its size.
        EquipmentPackage equipmentPackage;
        for (Map.Entry<String, Integer> requestedEntry :
                requestedEquipment.entrySet()) {

            String requestedType = requestedEntry.getKey();
            Integer requestedAmount = requestedEntry.getValue();

            ListIterator<EquipmentPackage> it =
                this.equipmentPackages.get(requestedType).listIterator();

            // Search for the closests matching overexceeding package size.
            boolean found = false,
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
            } else {  // Purchase equipment - Charge budget, do statistics, and add to repo.
                int amount = equipmentPackage.getAmount();
                statistics.addPurchasedEquipment(equipmentPackage);
                equipmentPackage.decrementAmount();
                repository.addItemToRepository(requestedType, amount);
            }
        }
    }


    public void purchaseScientists (
            Repository repository,
            Statistics statistics,
            Map<String, Integer> requestedScientists) {

        // Iterate over each requested specialization,
        // and search for the cheapest scientist of that specialization.
        Scientist scientist;
        for (Map.Entry<String, Integer> requestedEntry :
                requestedScientists.entrySet()) {

            String requestedSpecialization = requestedEntry.getKey();
            Integer requestedAmount = requestedEntry.getValue();

            // Test is there are any scientist of requestd spec.
            boolean missing = false;
            if (this.scientists.containsKey(requestedSpecialization)
                && this.scientists.get(
                    RequestedType).size() >= requestedAmount) {

                // Keep buying scientist from specialization until we get the
                // amount we need.
                Iterator<Scientist> it = this.scientists.get(
                        requestedSpecialization).iterator();
                while (it.hasNext() && requestedAmount > 0) {
                    scientist = it.next();

                    int price = scientist.getPrice();
                    statistics.addPurchasedScientist(scientist);
                    repository.addItemToRepository(
                            requestedSpecialization, amount);

                    requestedAmount --;
                    it.remove();
                }

                // Flag in case there aren't enough scientists from this
                // specialization in store.
                if (requestedAmount > 0) {
                    missing = true;
                }
            // In case there aren't any scientist in requesetd specialization.
            } else {
                missing = true;
            }

            if (missing) {
                System.out.println(
                    "Science Store: Not enough scientists for requested specialization '"
                    + requestedSpecialization + "'.");
            }
        }
    }


    public void purchaseLaboratory (
            Repository repository,
            Statistics statistics,
            String requestedLaboratory) {

        // Iterate over each requested specialization,
        // and search for the cheapest scientist of that specialization.
        Scientist laboratory;
        for (Map.Entry<String, Integer> requestedEntry :
                requestedLaboratory.entrySet()) {

            String requestedType = requestedEntry.getKey();
            Integer requestedAmount = requestedEntry.getValue();

            boolean found = false;
            if (this.scientiests.get(requestedType) >= requestedAmount) {
                found = true;
            }

            if ( ! found ) {
                System.out.println(
                        "Science Store: No matching specialization for requested scientist '"
                        + requestedType + "'.");
            } else {
                int price = scientist.getPrice();
                statistics.chargePrice(price);
                equipmentPackage.decrementAmount();
                repository.addItemToRepository(requestedType, amount);
            }
        }
    }


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
