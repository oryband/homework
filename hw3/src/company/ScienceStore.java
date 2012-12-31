/**
 * Represents a Science Store.
 *
 * @author Eldar Damari, Ory Band
 */
package company;


import java.util.Comparator;

import java.util.Collections;
import java.util.Map;
import java.util.HashMap; 
import java.util.ArrayList; 


public class ScienceStore implements ScienceStoreInterface {  
 
    private HashMap<String, ArrayList<EquipmentPackage>> equipmentPackages;
    private HashMap<String, ArrayList<Scientist>> scientists;
    private HashMap<String, ArrayList<Laboratory>> laboratories;


    public ScienceStore( 
            HashMap<String, ArrayList<EquipmentPackage>> equipmentPackages,
            HashMap<String, ArrayList<Scientist>> scientists,
            HashMap<String, ArrayList<Laboratory>> laboratories) {

        // FIXME Produces warnings since arguments are unchecked.
        this.equipmentPackages = new HashMap(equipmentPackages);
        this.scientists = new HashMap(scientists);
        this.laboratories = new HashMap(laboratories);

        // Sort equipment (amount) and scientists (price) from largest to smallest.
        for (Map.Entry<String, ArrayList<EquipmentPackage>> entry : this.equipmentPackages.entrySet()) {
            Collections.sort(entry.getValue());
            Collections.reverse(entry.getValue());
        }

        for (Map.Entry<String, ArrayList<Scientist>> entry : this.scientists.entrySet()) {
            Collections.sort(entry.getValue());
            Collections.reverse(entry.getValue());
        }
    }

    // Purchasing 
    // TODO 
    public boolean purchaseEquipmentPackages (
            Statistics statistics, Map<String, Integer> requestedEquipment) {

        Iterator<EquipmentPackage> it_equipmentPackages =
            this.equipmentPackages.get(requestedEquipment);

        // Edge case: Equipment TYPE isn't availble in store at all.
        if (it_equipmentPackages == null) {
            return false;
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
