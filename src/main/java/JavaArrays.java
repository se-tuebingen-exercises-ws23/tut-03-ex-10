import java.util.List;
import java.util.ArrayList;

public class JavaArrays {
    public static void kaboom() {
        // In Java: String <: Object.
        String[] strings = new String[2];
        Object[] objects = strings;  // This is valid, since in Java, arrays are covariant, i.e. String[] <: Object[]

        // P R O B L E M
        objects[0] = 12; // This is valid, since Int <: Object.
        // But it this causes a runtime exception `java.lang.ArrayStoreException: java.lang.Integer`

        // TODO: Verify ^ by running `sbt console` and typing `JavaArrays.kaboom()`

        // TODO: Why are arrays invariant in Scala?

        // TODO: Why are lists covariant in Scala?

        // Note: Java's arrays are covariant because of backwards compatibility
        // from the time when Java didn't have generics.
    }
}
