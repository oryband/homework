/**
 * Tests for ScienceStore class.
 *
 * @author Eldar Damari, Ory Band
 */

package company.tests;

import org.junit.*;


public class TestScienceStore {

    private ScienceStore store;

    @Before
    public static void setup() {
        // TODO Create hashmaps for constructor.
        ScienceStore store = new ScienceStore(/*TODO add them here*/);
        // NOTE Statistics will be tested in its own specific file.
        Statistics statistics = new Statistics(/*TODO Add budget, etc.*/);
    }

    @Test
    public final void testPurchaseEquipmentPackage() {
        store.PurchaseEquipmentPackage(statistics, "Microscope", 10);
    }

    @Test(expected= RuntimeException.class)
    public final void testMissingTypePurchaseEquipmentPackage() {
        store.purchaseEquipmentPackage(statistics, "MISSING", 10);
    }

    @Test(expected= RuntimeException.class)
    public final void testMissingAmountPurchaseEquipmentPackage() {
        store.purchaseEquipmentPackage(statistics, "Microscope", 5);
    }

    @Test(expected= RuntimeException.class)
    public final void testPurchaseTooManyEquipmentPackage() {
        store.purchaseEquipmentPackage(statistics, "Microscope", 10, 100);
        store.purchaseEquipmentPackage(statistics, "Microscope", 10, 100);
        store.purchaseEquipmentPackage(statistics, "Microscope", 10, 100);
    }


    @Test
    public final void testPurchaseScientist() {
        store.PurchaseScientist(statistics, "Family", "Steve");
    }

    @Test(expected= RuntimeException.class)
    public final void testMissingSpecializationPurchaseScientist() {
        store.PurchaseScientist(statistics, "MISSING", "Steve");
    }

    @Test(expected= RuntimeException.class)
    public final void testMissingNamePurchaseScientist() {
        store.PurchaseScientist(statistics, "Children", "NO_NAME");
    }

    @Test(expected= RuntimeException.class)
    public final void testPurchaseTooManyScientist() {
        store.purchaseScientist(statistics, "Family", "Steve");
        store.purchaseScientist(statistics, "Family", "Steve");
    }


    @Test
    public final void testPurchaseLaboratory() {
        store.PurchaseLaboratory(statistics, "House", "Family");
    }

    @Test(expected= RuntimeException.class)
    public final void testMissingHeadOfLaboratoryPurchaseLaboratory() {
        store.PurchaseLaboratory(statistics, "NO_NAME", "Family");
    }

    @Test(expected= RuntimeException.class)
    public final void testMissingSpecializationPurchaseLaboratory() {
        store.PurchaseLaboratory(statistics, "House", "MISSING");
    }

    @Test(expected= RuntimeException.class)
    public final void testPurchaseTooManyLaboratories() {
        store.PurchaseLaboratory(specialization"House", "Family");
        store.PurchaseLaboratory(specialization"House", "Family");
    }


    public static void main(String[] args) {
        Result result = JUnitCore.runClasses(MyClassTest.class);

        for (Failure failure : result.getFailures()) {
            System.out.println(failure.toString());
        }
    }
}
