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

    // Constructor
    public ScienceStore( 
            HashMap<String, EquipmentPackage> equipmentPackages,
            HashMap<String, Scientist> scientists,
            HashMap<String, Laboratory> laboratories) {}
    // Purchasing 
    public boolean purchaseEquipmentPackage(
            Statistics statistics, String requestedEquipment, int requestedAmount) {}
    public boolean purchaseScientist(
            Statistics statistics, String requestedSpecialization) {}
    public boolean purchaseLaboratory(
            Statistics statistics, String requestedSpecialization) {}

    // Calcilate best choise    
    private EquipmentPackage calculateBestPackage(
            String requestedEquipment, int requestedAmount) {}
    private Scientist calculateBestScientist(String requestedSpecialization) {}
    private Laboratory calculateBestLaboratory(String requestedSpecialization) {}
    
    // Getters
    public HashMap<String, ArrayList<EquipmentPackage>> 
                                        getEquipmentPackages() {
             return this.equipmentPackages;
            }
    public HashMap<String, ArrayList<EquipmentPackage>> 
                                        getScientists() {
             return this.scientists;
            }
    public HashMap<String, ArrayList<EquipmentPackage>> 
                                        getLaboratories() {
             return this.laboratories;
            }
    
    // For tests
    private boolean isEquipmentPackageEmpty(EquipmentPackage EquipmentPackage) {}
    private boolean isScientistsEmpty(String specialization) {}
    private boolean isLaboratoriesEmpty(String specialization) {}
}
