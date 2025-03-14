package it.ciamb;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * A wrapper class for declarative handling of boolean values.
 * <p>
 * Provides a fluent API for operating on booleans in a more readable
 * and functional way, avoiding explicit if-else constructs.
 * </p>
 *
 * <h3>Usage Example:</h3>
 * <pre>{@code
 * BooleanWrapper.of(true)
 *     .ifTrue(() -> System.out.println("It's true!"))
 *     .ifFalse(() -> System.out.println("It's false!"))
 *     .orElse(() -> System.out.println("It's null!"));
 * }</pre>
 */
public class BooleanWrapper {
    private final Boolean value;

    private BooleanWrapper(Boolean value) {
        this.value = value;
    }

    /**
     * Creates a {@code BooleanWrapper} instance with the given value.
     *
     * @param value the boolean value to wrap
     * @return a {@code BooleanWrapper} instance
     */
    public static BooleanWrapper of(Boolean value) {
        return new BooleanWrapper(value);
    }

    /**
     * Checks if the wrapped boolean value is {@code true}.
     *
     * @return {@code true} if the value is {@code true}, {@code false} otherwise
     */
    public boolean isTrue() {
        return Boolean.TRUE.equals(value);
    }

    /**
     * Checks if the wrapped boolean value is {@code false}.
     *
     * @return {@code true} if the value is {@code false}, {@code false} otherwise
     */
    public boolean isFalse() {
        return Boolean.FALSE.equals(value);
    }

    /**
     * Checks if the wrapped boolean value is present (not {@code null}).
     *
     * @return {@code true} if the value is non-null, {@code false} otherwise
     */
    public boolean isPresent() {
        return value != null;
    }

    /**
     * Executes the given action if the wrapped value is {@code true}.
     *
     * @param action the action to execute if the value is {@code true}
     * @return this {@code BooleanWrapper} instance for chaining
     * @throws IllegalArgumentException if {@code action} is {@code null}
     */
    public BooleanWrapper ifTrue(Runnable action) {
        if (action == null)
            throw new IllegalArgumentException("action cannot be null");
        if (Boolean.TRUE.equals(value))
            action.run();
        return this;
    }

    /**
     * Executes the given action if the wrapped value is {@code false}.
     *
     * @param action the action to execute if the value is {@code false}
     * @return this {@code BooleanWrapper} instance for chaining
     * @throws IllegalArgumentException if {@code action} is {@code null}
     */
    public BooleanWrapper ifFalse(Runnable action) {
        if (action == null)
            throw new IllegalArgumentException("action cannot be null");
        if (Boolean.FALSE.equals(value))
            action.run();
        return this;
    }

    /**
     * Executes the given action if the wrapped value is present (not {@code null}).
     *
     * @param action the action to execute with the present boolean value
     * @return this {@code BooleanWrapper} instance for chaining
     * @throws IllegalArgumentException if {@code action} is {@code null}
     */
    public BooleanWrapper ifPresent(Consumer<Boolean> action) {
        if (action == null)
            throw new IllegalArgumentException("action cannot be null");
        if (value != null)
            action.accept(value);
        return this;
    }

    /**
     * Executes the given action if the wrapped value is {@code null}.
     *
     * @param action the action to execute if the value is {@code null}
     * @return this {@code BooleanWrapper} instance for chaining
     * @throws IllegalArgumentException if {@code action} is {@code null}
     */
    public BooleanWrapper orElse(Runnable action) {
        if (action == null)
            throw new IllegalArgumentException("action cannot be null");
        if (value == null)
            action.run();
        return this;
    }

    /**
     * Returns the wrapped boolean value or a default value if it is {@code null}.
     *
     * @param defaultValue the default value to return if the wrapped value is {@code null}
     * @return the wrapped value or {@code defaultValue} if it is {@code null}
     */
    public boolean orElse(boolean defaultValue) {
        return Optional.ofNullable(value).orElse(defaultValue);
    }

    /**
     * Returns the wrapped boolean value or a supplied value if it is {@code null}.
     *
     * @param supplier the supplier providing the default boolean value if {@code null}
     * @return the wrapped value or the value provided by the supplier if {@code null}
     * @throws IllegalArgumentException if {@code supplier} is {@code null}
     */
    public boolean orElseGet(Supplier<Boolean> supplier) {
        if (supplier == null)
            throw new IllegalArgumentException("supplier cannot be null");
        return value != null ? value : supplier.get();
    }

    /**
     * Performs a logical AND operation between the current value and another {@code BooleanWrapper}.
     *
     * @param other the other {@code BooleanWrapper} to combine with AND
     * @return a new {@code BooleanWrapper} containing the result of the AND operation
     */
    public BooleanWrapper and(BooleanWrapper other) {
        if (other == null || other.value == null || this.value == null)
            return BooleanWrapper.of(null);
        return new BooleanWrapper(this.value && other.value);
    }

    /**
     * Performs a logical OR operation between the current value and another {@code BooleanWrapper}.
     *
     * @param other the other {@code BooleanWrapper} to combine with OR
     * @return a new {@code BooleanWrapper} containing the result of the OR operation
     */
    public BooleanWrapper or(BooleanWrapper other) {
        if (other == null || other.value == null)
            return this;
        if (this.value == null)
            return other;
        return new BooleanWrapper(this.value || other.value);
    }

    /**
     * Performs a logical XOR operation between the current value and another {@code BooleanWrapper}.
     *
     * @param other the other {@code BooleanWrapper} to combine with XOR
     * @return a new {@code BooleanWrapper} containing the result of the XOR operation,
     *         or {@code null} if either value is {@code null}
     */
    public BooleanWrapper xor(BooleanWrapper other) {
        if (other == null || other.value == null || this.value == null)
            return BooleanWrapper.of(null);
        return new BooleanWrapper(this.value ^ other.value);
    }

    /**
     * Returns the negated boolean value.
     *
     * @return a new {@code BooleanWrapper} with the negated value,
     *         or a {@code BooleanWrapper} with {@code null} if the value is {@code null}
     */
    public BooleanWrapper not() {
        return value != null ? new BooleanWrapper(!value) : BooleanWrapper.of(null);
    }

    /**
     * Applies a transformation function to the boolean value and returns an {@code Optional}
     * containing the result.
     *
     * @param <T>    the type of the transformed value
     * @param mapper the transformation function
     * @return an {@code Optional} containing the result of the transformation,
     *         or an empty {@code Optional} if the value is {@code null}
     * @throws IllegalArgumentException if {@code mapper} is {@code null}
     */
    public <T> Optional<T> map(Function<Boolean, T> mapper) {
        if (mapper == null)
            throw new IllegalArgumentException("mapping function cannot be null");
        return Optional.ofNullable(value).map(mapper);
    }

    /**
     * Returns the wrapped boolean value.
     *
     * @return the boolean value, which could be {@code null}
     */
    public Boolean get() {
        return value;
    }

    /**
     * Returns the primitive boolean value.
     *
     * @return the wrapped boolean value if present, otherwise {@code false} as a default value
     */
    public boolean toPrimitive() {
        return value != null && value;
    }

}
