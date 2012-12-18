/**
 * Tests for ScienceStore class.
 *
 * @author Eldar Damari, Ory Band
 */
package company.tests;

import org.junit.*;


public class TestScienceStore {

    private ScienceStore store;

    //@BeforeClass
    //public static void oneTimeSetup() {}

    //@AfterClass
    //public static void oneTimeTearDown() {}

    @Before
    public static void setup() {
        // TODO Create hashmaps for constructor.
        ScienceStore store = new ScienceStore(/*TODO add them here*/);
    }

    //@After
    //public static void teardown() {}

    @Test
    public final void testPurchaseEquipmentPackage() {
        store.PurchaseEquipmentPackage("Microscope", 10);
    }

    @Test(expected= RuntimeException.class)
    public final void testMissingTypePurchaseEquipmentPackage() {
        store.purchaseEquipmentPackage("MISSING", 10);
    }

    @Test(expected= RuntimeException.class)
    public final void testMissingAmountPurchaseEquipmentPackage() {
        store.purchaseEquipmentPackage("Microscope", 5);
    }

    @Test(expected= RuntimeException.class)
    public final void testPurchaseTooManyEquipmentPackage() {
        store.purchaseEquipmentPackage("Microscope", 10, 100);
        store.purchaseEquipmentPackage("Microscope", 10, 100);
        store.purchaseEquipmentPackage("Microscope", 10, 100);
    }


    @Test
    public final void testPurchaseScientist() {
        store.PurchaseScientist("Family", "Steve");
    }

    @Test(expected= RuntimeException.class)
    public final void testMissingSpecializationPurchaseScientist() {
        store.PurchaseScientist("MISSING", "Steve");
    }

    @Test(expected= RuntimeException.class)
    public final void testMissingNamePurchaseScientist() {
        store.PurchaseScientist("Children", "NO_NAME");
    }

    @Test(expected= RuntimeException.class)
    public final void testPurchaseTooManyScientist() {
        store.purchaseScientist("Family", "Steve");
        store.purchaseScientist("Family", "Steve");
    }


    @Test
    public final void testPurchaseLaboratory() {
        store.PurchaseLaboratory("House", "Family");
    }

    @Test(expected= RuntimeException.class)
    public final void testMissingHeadOfLaboratoryPurchaseLaboratory() {
        store.PurchaseLaboratory("NO_NAME", "Family");
    }

    @Test(expected= RuntimeException.class)
    public final void testMissingSpecializationPurchaseLaboratory() {
        store.PurchaseLaboratory("House", "MISSING");
    }

    @Test(expected= RuntimeException.class)
    public final void testPurchaseTooManyLaboratories() {
        store.PurchaseLaboratory("House", "Family");
        store.PurchaseLaboratory("House", "Family");
    }


    public static void main(String[] args) {
        Result result = JUnitCore.runClasses(MyClassTest.class);

        for (Failure failure : result.getFailures()) {
            System.out.println(failure.toString());
        }
    }
}
