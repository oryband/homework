/**
 * Represents a Science Store.
 *
 * @author Eldar Damari, Ory Band
 */
//package company;


import java.util.Comparator;

import java.util.Collections;
import java.util.Iterator;
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
        // and sorts scientists & labs from cheapest to most expensive.
        for (Map.Entry<String, ArrayList<EquipmentPackage>> entry
                : this.equipmentPackages.entrySet()) {

            Collections.sort(entry.getValue());
            Collections.reverse(entry.getValue());
        }

        for (Map.Entry<String, ArrayList<Scientist>> entry :
                this.scientists.entrySet()) {

            Collections.sort(entry.getValue());
        }

        for (Map.Entry<String, ArrayList<Laboratory>> entry :
                this.laboratories.entrySet()) {

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
                repository.addEquipmentToRepository(requestedType, amount);
            }
        }
    }


    public void purchaseScientists (
            HeadOfLaboratory headOfLaboratory,
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
                    requestedSpecialization).size() >= requestedAmount) {

                // Keep buying scientist from specialization until we get the
                // amount we need.
                Iterator<Scientist> it = this.scientists.get(
                        requestedSpecialization).iterator();
                while (it.hasNext() && requestedAmount > 0) {
                    scientist = it.next();

                    int price = scientist.getPrice();
                    statistics.addPurchasedScientist(scientist);
                    headOfLaboratory.addScientists(1);

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
            ChiefScientist chiefScientist,
            Statistics statistics,
            String requestedSpecialization) {

        // Test if is there are is a laboratory from the specialization we
        // need.
        if (this.laboratories.containsKey(requestedSpecialization)) {
            ArrayList<Laboratory> laboratories =
                this.laboratories.get(requestedSpecialization);

            // getting the lab
            Laboratory laboratory = laboratories.get(0);

            // TODO Because we delete the lab and pointer sent to statistics,
            // need to see that all working fine.
            statistics.addPurchasedLaboratory(laboratory);

            int price = laboratory.getPrice();
            String name = laboratory.getName();
            String specialization = laboratory.getSpecialization();
            int numberOfScientists = laboratory.getNumOfScientists();

            // creating instance of head of laboratory to be used as
            // laboratory in chief.
            HeadOfLaboratory headOfLaboratory = new HeadOfLaboratory(
                    name, specialization, numberOfScientists);

            chiefScientist.addLaboratory(headOfLaboratory);

            // need to delete laboratory from store!!!
            laboratories.remove(0);// removing lab from store
        } else {
            System.out.println(
                    "Science Store: No laboratory for requested specialization '"
                    + requestedSpecialization + "'.");
        }
    }
}
