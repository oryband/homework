/**
 * Represents a Science Store.
 *
 * @author Eldar Damari, Ory Band
 */
package company;

import java.util.HashMap; 
import java.util.ArrayList; 


public class ScienceStore implements ScienceStoreInterface {  
 
    private HashMap<String, ArrayList<EquipmentPackage>> equipmentPackages;
    private HashMap<String, ArrayList<Scientist>> scientists;
    private HashMap<String, ArrayList<Laboratory>> laboratories;

    public ScienceStore( 
            HashMap<String, EquipmentPackage> equipmentPackages,
            HashMap<String, Scientist> scientists,
            HashMap<String, Laboratory> laboratories) {}

    public boolean purchaseEquipmentPackage(
            Statistics statistics, String requestedEquipment, int requestedAmount) {}
    public boolean purchaseScientist(
            Statistics statistics, String requestedSpecialization) {}
    public boolean purchaseLaboratory(
            Statistics statistics, String requestedSpecialization) {}
    
    private EquipmentPackage calculateBestPackage(
            String requestedEquipment, int requestedAmount) {}
    private Scientist calculateBestScientist(String requestedSpecialization) {}
    private Laboratory calculateBestLaboratory(String requestedSpecialization) {}

    private boolean isEquipmentPackageEmpty(EquipmentPackage EquipmentPackage) {}
    private boolean isScientistsEmpty(String specialization) {}
    private boolean isLaboratoriesEmpty(String specialization) {}
}
