/**
 * Science Store interface.
 *
 * @author Eldar Damari, Ory Band
 */
package company;

import stat.*;
import utilities.*;

public interface ScienceStoreInterface {

    /** 
     * Substracts equipment package if found, and charges money from budget.
     *
     * @param statistics for handling purchase.
     * @param requestedEquipment equipment type (Microscope, burner, etc.)
     * @param requestedAmount requested equipment amount.
     *
     * @return true if package is in stock, false otherwise.
     *
     * @pre statistics != null
     * @pre requestedEquipment != null
     * @pre requestedAmount > 0 
     * @pre ! isEquipmentPackageEmpty(requestedEquipment)
     *
     * @post ! isEquipmentPackageEmpty(requestedEquipment)
     **/
    public boolean purchaseEquipmentPackage(
            Statistics statistics, String requestedEquipment, int requestedAmount);
    
    /**
     * Substracts scientist if found, and charges money from budget.
     *
     * @param statistics for handling purchase.
     * @param requestedSpecialization scientist specialization.
     *
     * @return true if science is in stock, false otherwise.
     *
     * @pre statistics != null
     * @pre requestedSpecialization != null
     * @pre ! isScientistsEmpty(requestedSpecialization)
     *
     * @post ! isScientistsEmpty(requestedSpecialization)
     **/
    public boolean purchaseScientist(
            Statistics statistics, String requestedSpecialization);
    
    /**
     * Substracts laboratory if found, and charges money from budget.
     *
     * @param statistics for handling purchase.
     * @param requestedSpecialization Lab specialization.
     *
     * @pre statistics != null
     * @pre requestedSpecialization != null
     * @pre ! isLaboratoriesEmpty(requestedSpecialization)
     *
     * @post ! isLaboratoriesEmpty(requestedSpecialization)
     **/
    public boolean purchaseLaboratory(
            Statistics statistics, String requestedSpecialization); 


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
