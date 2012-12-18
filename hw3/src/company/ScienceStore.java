/**
 * Represents a Science Store
 *
 * @author Eldar Damari, Ory Band
 */
package company;


import java.util.HashMap; 


public class ScienceStore {
    private HashMap<String, EquipmentPackage> equipmentPackages;
    private HashMap<String, Scientist> scientists;
    private HashMap<String, Laboratory> laboratories;

    public ScienceStore( 
            HashMap<String, EquipmentPackage> equipmentPackages,
            HashMap<String, Scientist> scientists,
            HashMap<String, Laboratory> laboratories) {};

    public void purchaseEquipmentPackage(String type, int amount) {}
    public void purchaseScientist(String specializaiton, String name) {}
    public void purchaseLaboratory(
            String headOfLaboratory, String specialization) {}
}
