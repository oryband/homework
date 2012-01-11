/**
 * Represents a group of Assignment (inteface).
 *
 * @author Ory Band
 * @version 1.0
 */
public interface Assignments {
    /**
     * @param var The variable whose value will be returned.
     *
     * @return variable var's value.
     */
    public double valueOf(Variable var);

    /**
     * Adds a new assignment.
     *
     * @param assignment The Assignment object to be added.
     */
    public void addAssignment(Assignment assignment);
}

