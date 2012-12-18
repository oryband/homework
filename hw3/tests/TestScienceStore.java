/**
 * Tests for ScienceStore class.
 *
 * @author Eldar Damari, Ory Band
 */
package company.tests;

import org.junit.*;


public class TestScienceStore {

    private ScienceStore store;

    @BeforeClass
    public static void oneTimeSetup() {
        return;
    }

    @AfterClass
    public static void oneTimeTearDown() {
        return;
    }

    @Before
    public static void setup() {
        ScienceStore store = new ScienceStore();
    }

    @After
    public static void teardown() {
        return;
    }

    // test case method name should start with test.
    @Test
    public final void testAdd() {
        assertEquals(calcEngine.add(20, 30), 50);
    }

    public static void main(String[] args) {
        Result result = JUnitCore.runClasses(MyClassTest.class);

        for (Failure failure : result.getFailures()) {
            System.out.println(failure.toString());
        }
    }
}
