/**
 * Represents a Science Store.
 *
 * @author Eldar Damari, Ory Band
 */
package company;

import java.util.Comparator;
import java.util.Map;
import java.util.HashMap; 
import java.util.ArrayList; 


public class ScienceStore implements ScienceStoreInterface {  
 
    private HashMap<String, ArrayList<EquipmentPackage>> equipmentPackages;
    private HashMap<String, ArrayList<Scientist>> scientists;
    private HashMap<String, ArrayList<Laboratory>> laboratories;


    public ScienceStore( 
            Map<String, EquipmentPackage> equipmentPackages,
            Map<String, Scientist> scientists,
            Map<String, Laboratory> laboratories) {

        this.scientists = new HashMap(scientists);
        this.laboratories = new HashMap(laboratories);

        this.equipmentPackages = new HashMap(equipmentPackages);

        // Sort equipment and scientists.
        HashMap.Entry pairs;
        for (EquipmentPackage p : this.equipmentPackages.values()) {
            Collections.sort(p);
        }

        for (Scientist s : this.scientists.values()) {
            Collections.sort(p);
        }
    }

    // Purchasing 
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
