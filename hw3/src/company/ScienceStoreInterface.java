/**
 * Science Store interface.
 *
 * @author Eldar Damari, Ory Band
 */
package company;

import java.util.Map;


public interface ScienceStoreInterface {

    /**
     * Substracts equipment package if found, and charges money from budget.
     *
     * @param statistics for handling purchase.
     * @param requestedEquipment equipment hash map {type : amount}
     *
     * @return true if package is in stock, false otherwise.
     **/
    public boolean purchaseEquipmentPackages (
            Statistics statistics, Map<String, Integer> requestedEquipment);

    /**
     * Substracts scientist if found, and charges money from budget.
     *
     * @param statistics for handling purchase.
     * @param requestedScientists scientist hash map {specialization : price}
     *
     * @return true if requested scientist are in stock, false otherwise.
     **/
    public boolean purchaseScientist (
            Statistics statistics, Map<String, Integer> requestedScientists);

    /**
     * Substracts laboratory if found, and charges money from budget.
     *
     * @param statistics for handling purchase.
     * @param requestedLaboratory Lab specialization.
     **/
    public boolean purchaseLaboratory (
            Statistics statistics, String requestedLaboratory);


    /**
     * @param requestedEquipment equipment type (Microscope, burner, etc.)
     *
     * @return True if EquipmentPackage is in store (not just in stock).
     **/
    public boolean isEquipmentPackageEmpty(EquipmentPackage EquipmentPackage);

    /**
     * @param requestedSpecialization Scientist specialization.
     *
     * @return True if scientist is in store (not just in stock).
     **/
    public boolean isScientistsEmpty(String specialization);

    /**
     * @param requestedSpecialization Laboratory specialization.
     *
     * @return True if laboratory is in store (not just in stock).
     **/
    public boolean isLaboratoriesEmpty(String specialization);
}
