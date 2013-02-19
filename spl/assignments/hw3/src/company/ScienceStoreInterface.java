/** @author Eldar Damari, Ory Band */

package company;

import java.util.Map;


public interface ScienceStoreInterface {

    /**
     * Find the best package size for each requested item and charges budget.
     * Best package size is the closest to the requested amount size that
     * overexceeds the requested size.
     *
     * @param repository for updating repository.
     * @param statistics for charging from the budget.
     * @param requestedEquipment shopping list {type : amount}
     *
     * @PRE EquipmentPackages are sorted from larget amount to smallest amount.
     */
    public void purchaseEquipmentPackages (
            Repository repository,
            Statistics statistics,
            Map<String, Integer> requestedEquipment);

    /**
     * Find the cheapest scientist for each requested specialization and
     * charges budget.
     *
     * @param repository for updating repository.
     * @param statistics for charging from the budget.
     * @param requestedScientiest shopping list {specialization : amount}
     *
     * @PRE Scientists are sorted from cheapest to most expensive.
     */
    public void purchaseScientists (
            HeadOfLaboratory headOfLaboratory,
            Statistics statistics,
            Map<String, Integer> requestedScientists);

    /**
     * Find the cheapest laboratory the requested specialization and charges
     * budget.
     *
     * @param repository for updating repository.
     * @param statistics for charging from the budget.
     * @param requestedLaboratory
     *
     * @PRE Labs are sorted from cheapest to most expensive
     */
    public void purchaseLaboratory (
            ChiefScientist chiefScientist,
            Statistics statistics,
            String requestedSpecialization);


    public String toString();
}
