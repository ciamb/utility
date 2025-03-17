package it.ciamb;

import org.junit.jupiter.api.Test;

import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Supplier;

import static org.junit.jupiter.api.Assertions.*;
class BooleanWrapperTest {

    /**
     * Test for of() method
     */
    @Test
    void of_shouldReturnWrapperTrue_whenValueIsTrue() {
        BooleanWrapper wrapper = BooleanWrapper.of(true);
        assertNotNull(wrapper);
        assertTrue(wrapper.isTrue());
        assertFalse(wrapper.isFalse());
        assertTrue(wrapper.isPresent());
        assertEquals(Boolean.TRUE, wrapper.get());
    }

    @Test
    void of_shouldReturnWrapperFalse_whenValueIsFalse() {
        BooleanWrapper wrapper = BooleanWrapper.of(false);
        assertNotNull(wrapper);
        assertFalse(wrapper.isTrue());
        assertTrue(wrapper.isFalse());
        assertTrue(wrapper.isPresent());
        assertEquals(Boolean.FALSE, wrapper.get());
    }

    @Test
    void of_shouldReturnWrapperNull_whenValueIsNull() {
        BooleanWrapper wrapper = BooleanWrapper.of(null);
        assertNotNull(wrapper);
        assertFalse(wrapper.isTrue());
        assertFalse(wrapper.isFalse());
        assertFalse(wrapper.isPresent());
        assertNull(wrapper.get());
    }

    /**
     * Test for isTrue() method
     */
    @Test
    void isTrue_shouldReturnTrue_whenValueIsTrue() {
        BooleanWrapper wrapper = BooleanWrapper.of(true);
        assertTrue(wrapper.isTrue(), "Must be true for true value");
    }

    @Test
    void isTrue_shouldReturnFalse_whenValueIsFalse() {
        BooleanWrapper wrapper = BooleanWrapper.of(false);
        assertFalse(wrapper.isTrue(), "Must be false for false value");
    }

    @Test
    void isTrue_shouldReturnFalse_whenValueIsNull() {
        BooleanWrapper wrapper = BooleanWrapper.of(null);
        assertFalse(wrapper.isTrue(), "Must be false for null value");
    }

    /**
     * Test for isFalse() method
     */
    @Test
    void isFalse_shouldReturnFalse_whenValueIsTrue() {
        BooleanWrapper wrapper = BooleanWrapper.of(true);
        assertFalse(wrapper.isFalse(), "Must be false for true value");
    }

    @Test
    void isFalse_shouldReturnTrue_whenValueIsFalse() {
        BooleanWrapper wrapper = BooleanWrapper.of(false);
        assertTrue(wrapper.isFalse(), "Must be true for false value");
    }

    @Test
    void isFalse_shouldReturnFalse_whenValueIsNull() {
        BooleanWrapper wrapper = BooleanWrapper.of(null);
        assertFalse(wrapper.isFalse(), "Must be false for null value");
    }

    /**
     * Test for isPresent() method
     */
    @Test
    void isPresent_shouldReturnTrue_whenValueIsTrue() {
        BooleanWrapper wrapper = BooleanWrapper.of(true);
        assertTrue(wrapper.isPresent(), "Must be true for true value");
    }

    @Test
    void isPresent_shouldReturnTrue_whenValueIsFalse() {
        BooleanWrapper wrapper = BooleanWrapper.of(false);
        assertTrue(wrapper.isPresent(), "Must be true for false value");
    }

    @Test
    void isPresent_shouldReturnFalse_whenValueIsNull() {
        BooleanWrapper wrapper = BooleanWrapper.of(null);
        assertFalse(wrapper.isPresent(), "Must be false for null value");
    }

    /**
     * Test for ifTrue() method
     */
    @Test
    void ifTrue_shouldExecuteAction_whenValueIsTrue() {
        AtomicBoolean wasExecuted = new AtomicBoolean(false);

        BooleanWrapper wrapper = BooleanWrapper.of(true);
        wrapper.ifTrue(() -> wasExecuted.set(true));

        assertTrue(wasExecuted.get(), "Expected action to be executed when value is true");
    }

    @Test
    void ifTrue_shouldNotExecuteAction_whenValueIsFalse() {
        AtomicBoolean wasExecuted = new AtomicBoolean(false);

        BooleanWrapper wrapper = BooleanWrapper.of(false);
        wrapper.ifTrue(() -> wasExecuted.set(true));

        assertFalse(wasExecuted.get(), "Expected action NOT to be executed when value is false");
    }

    @Test
    void ifTrue_shouldNotExecuteAction_whenValueIsNull() {
        AtomicBoolean wasExecuted = new AtomicBoolean(false);

        BooleanWrapper wrapper = BooleanWrapper.of(null);
        wrapper.ifTrue(() -> wasExecuted.set(true));

        assertFalse(wasExecuted.get(), "Expected action NOT to be executed when value is null");
    }

    @Test
    void ifTrue_shouldThrowException_whenActionIsNull() {
        BooleanWrapper wrapper = BooleanWrapper.of(true);

        assertThrows(IllegalArgumentException.class,
                () -> wrapper.ifTrue(null),
                "Expected IllegalArgumentException when action is null");
    }

    /**
     * Test for ifFalse() method
     */
    @Test
    void ifFalse_shouldExecuteAction_whenValueIsFalse() {
        AtomicBoolean wasExecuted = new AtomicBoolean(false);

        BooleanWrapper wrapper = BooleanWrapper.of(false);
        wrapper.ifFalse(() -> wasExecuted.set(true));

        assertTrue(wasExecuted.get(), "Expected action to be executed when value is false");
    }

    @Test
    void ifFalse_shouldNotExecuteAction_whenValueIsTrue() {
        AtomicBoolean wasExecuted = new AtomicBoolean(false);

        BooleanWrapper wrapper = BooleanWrapper.of(true);
        wrapper.ifFalse(() -> wasExecuted.set(true));

        assertFalse(wasExecuted.get(), "Expected action NOT to be executed when value is true");
    }

    @Test
    void ifFalse_shouldNotExecuteAction_whenValueIsNull() {
        AtomicBoolean wasExecuted = new AtomicBoolean(false);

        BooleanWrapper wrapper = BooleanWrapper.of(null);
        wrapper.ifFalse(() -> wasExecuted.set(true));

        assertFalse(wasExecuted.get(), "Expected action NOT to be executed when value is null");
    }

    @Test
    void ifFalse_shouldThrowException_whenActionIsNull() {
        BooleanWrapper wrapper = BooleanWrapper.of(false);

        assertThrows(IllegalArgumentException.class,
                () -> wrapper.ifFalse(null),
                "Expected IllegalArgumentException when action is null");
    }

    /**
     * Test for ifPresent() method
     */
    @Test
    void ifPresent_shouldExecuteAction_whenValueIsTrue() {
        AtomicBoolean wasExecuted = new AtomicBoolean(false);

        BooleanWrapper wrapper = BooleanWrapper.of(true);
        wrapper.ifPresent(value -> wasExecuted.set(true));

        assertTrue(wasExecuted.get(), "Expected action to be executed when value is present");
    }

    @Test
    void ifPresent_shouldExecuteAction_whenValueIsFalse() {
        AtomicBoolean wasExecuted = new AtomicBoolean(false);

        BooleanWrapper wrapper = BooleanWrapper.of(false);
        wrapper.ifPresent(value -> wasExecuted.set(true));

        assertTrue(wasExecuted.get(), "Expected action to be executed when value is present");
    }

    @Test
    void ifPresent_shouldNotExecuteAction_whenValueIsNull() {
        AtomicBoolean wasExecuted = new AtomicBoolean(false);

        BooleanWrapper wrapper = BooleanWrapper.of(null);
        wrapper.ifPresent(value -> wasExecuted.set(true));

        assertFalse(wasExecuted.get(), "Expected action NOT to be executed when value is null");
    }

    @Test
    void ifPresent_shouldThrowException_whenActionIsNull() {
        BooleanWrapper wrapper = BooleanWrapper.of(true);

        assertThrows(IllegalArgumentException.class, 
                () -> wrapper.ifPresent(null),
                "Expected IllegalArgumentException when action is null");
    }

    /**
     * Test for orElse() method
     */
    @Test
    void orElse_shouldSetTrueValue_whenActionIsPresent() {
        AtomicBoolean wasExecuted = new AtomicBoolean(false);

        BooleanWrapper wrapper = BooleanWrapper.of(null);
        wrapper.orElse(() -> wasExecuted.set(true));

        assertTrue(wasExecuted.get(), "Expected to be true after orElse");
    }

    @Test
    void orElse_shouldThrowIllegalArgumentException_whenActionIsNull() {
        BooleanWrapper wrapper = BooleanWrapper.of(true);

        assertThrows(IllegalArgumentException.class,
                () -> wrapper.orElse(null),
                "Expected IllegalArgumentException when action is null");
    }

    @Test
    void orElse_shouldReturnWrappedValue_whenNotNull() {
        BooleanWrapper wrapper = BooleanWrapper.of(true);

        boolean orElse = wrapper.orElse(false);

        assertTrue(orElse, "Expected true because the wrapped value is not null");
    }

    @Test
    void orElse_shouldReturnDefaultValue_whenNull() {
        BooleanWrapper wrapper = BooleanWrapper.of(null);

        boolean orElse = wrapper.orElse(false);

        assertFalse(orElse, "Expected false because the wrapped value is null");
    }

    /**
     * Test for orElseGet() method
     */
    @Test
    void orElseGet_shouldThrowIllegalArgumentException_whenSupplierIsNull() {
        BooleanWrapper wrapper = BooleanWrapper.of(true);

        assertThrows(IllegalArgumentException.class,
                () -> wrapper.orElseGet(null),
                "Should throw IllegalArgumentException when given supplier is null");
    }

    @Test
    void orElseGet_shouldSetOrElseGetValue_whenInitialValueIsNull() {
        BooleanWrapper wrapper = BooleanWrapper.of(null);

        boolean orElseGet = wrapper.orElseGet(() -> true);

        assertTrue(orElseGet, "Expected to be true because initial value is null");
    }

    @Test
    void orElseGet_shouldNotSetOrElseGetValue_whenInitialValueIsPresent() {
        BooleanWrapper wrapper = BooleanWrapper.of(true);

        boolean orElseGet = wrapper.orElseGet(() -> false);

        assertTrue(orElseGet, "Expected to be true because initial value is present");
    }

    /**
     * Test for and() method
     */
    @Test
    void and_shouldReturnTrue_whenBothValuesAreTrue() {
        BooleanWrapper a = BooleanWrapper.of(true);
        BooleanWrapper b = BooleanWrapper.of(true);
        BooleanWrapper result = a.and(b);
        assertNotNull(result.get(), "Expected non-null result");
        assertTrue(result.get(), "Expected true because both values are true");
    }

    @Test
    void and_shouldReturnFalse_whenOneValueIsFalse() {
        BooleanWrapper a = BooleanWrapper.of(true);
        BooleanWrapper b = BooleanWrapper.of(false);
        BooleanWrapper result = a.and(b);
        assertNotNull(result.get(), "Expected non-null result");
        assertFalse(result.get(), "Expected false because one value is false");
    }

    @Test
    void and_shouldReturnFalse_whenBothValuesAreFalse() {
        BooleanWrapper a = BooleanWrapper.of(false);
        BooleanWrapper b = BooleanWrapper.of(false);
        BooleanWrapper result = a.and(b);
        assertNotNull(result.get(), "Expected non-null result");
        assertFalse(result.get(), "Expected false because both values are false");
    }

    @Test
    void and_shouldReturnNull_whenFirstValueIsNull() {
        BooleanWrapper a = BooleanWrapper.of(null);
        BooleanWrapper b = BooleanWrapper.of(true);
        BooleanWrapper result = a.and(b);
        assertNull(result.get(), "Expected null because the first value is null");
    }

    @Test
    void and_shouldReturnNull_whenSecondValueIsNull() {
        BooleanWrapper a = BooleanWrapper.of(true);
        BooleanWrapper b = BooleanWrapper.of(null);
        BooleanWrapper result = a.and(b);
        assertNull(result.get(), "Expected null because the second value is null");
    }

    @Test
    void and_shouldReturnNull_whenBothValuesAreNull() {
        BooleanWrapper a = BooleanWrapper.of(null);
        BooleanWrapper b = BooleanWrapper.of(null);
        BooleanWrapper result = a.and(b);
        assertNull(result.get(), "Expected null because both values are null");
    }

    @Test
    void and_shouldReturnNull_whenOtherIsNull() {
        BooleanWrapper a = BooleanWrapper.of(true);
        BooleanWrapper result = a.and(null);
        assertNull(result.get(), "Expected null because the other BooleanWrapper is null");
    }

    /**
     * Test for or() method
     */
    @Test
    void or_shouldReturnTrue_whenBothValuesAreTrue() {
        BooleanWrapper a = BooleanWrapper.of(true);
        BooleanWrapper b = BooleanWrapper.of(true);
        BooleanWrapper result = a.or(b);
        assertNotNull(result.get(), "Expected non-null result");
        assertTrue(result.get(), "Expected true because both values are true");
    }

    @Test
    void or_shouldReturnTrue_whenOneValueIsTrue() {
        BooleanWrapper a = BooleanWrapper.of(true);
        BooleanWrapper b = BooleanWrapper.of(false);
        BooleanWrapper result = a.or(b);
        assertNotNull(result.get(), "Expected non-null result");
        assertTrue(result.get(), "Expected true because one value is true");

        BooleanWrapper c = BooleanWrapper.of(false);
        BooleanWrapper d = BooleanWrapper.of(true);
        BooleanWrapper result2 = c.or(d);
        assertNotNull(result2.get(), "Expected non-null result");
        assertTrue(result2.get(), "Expected true because one value is true");
    }

    @Test
    void or_shouldReturnFalse_whenBothValuesAreFalse() {
        BooleanWrapper a = BooleanWrapper.of(false);
        BooleanWrapper b = BooleanWrapper.of(false);
        BooleanWrapper result = a.or(b);
        assertNotNull(result.get(), "Expected non-null result");
        assertFalse(result.get(), "Expected false because both values are false");
    }

    @Test
    void or_shouldReturnOther_whenCurrentValueIsNull() {
        BooleanWrapper a = BooleanWrapper.of(null);
        BooleanWrapper b = BooleanWrapper.of(true);
        BooleanWrapper result = a.or(b);
        assertNotNull(result.get(), "Expected non-null result");
        assertTrue(result.get(), "Expected true because the first value is null and the second is true");

        BooleanWrapper c = BooleanWrapper.of(null);
        BooleanWrapper d = BooleanWrapper.of(false);
        BooleanWrapper result2 = c.or(d);
        assertNotNull(result2.get(), "Expected non-null result");
        assertFalse(result2.get(), "Expected false because the first value is null and the second is false");
    }

    @Test
    void or_shouldReturnThis_whenOtherValueIsNull() {
        BooleanWrapper a = BooleanWrapper.of(true);
        BooleanWrapper b = BooleanWrapper.of(null);
        BooleanWrapper result = a.or(b);
        assertNotNull(result.get(), "Expected non-null result");
        assertTrue(result.get(), "Expected true because the second value is null and the first is true");

        BooleanWrapper c = BooleanWrapper.of(false);
        BooleanWrapper d = BooleanWrapper.of(null);
        BooleanWrapper result2 = c.or(d);
        assertNotNull(result2.get(), "Expected non-null result");
        assertFalse(result2.get(), "Expected false because the second value is null and the first is false");
    }

    @Test
    void or_shouldReturnThis_whenOtherIsNull() {
        BooleanWrapper a = BooleanWrapper.of(true);
        BooleanWrapper result = a.or(null);
        assertNotNull(result.get(), "Expected non-null result");
        assertTrue(result.get(), "Expected true because the other BooleanWrapper is null");

        BooleanWrapper b = BooleanWrapper.of(false);
        BooleanWrapper result2 = b.or(null);
        assertNotNull(result2.get(), "Expected non-null result");
        assertFalse(result2.get(), "Expected false because the other BooleanWrapper is null");
    }

    @Test
    void or_shouldReturnNull_whenBothValuesAreNull() {
        BooleanWrapper a = BooleanWrapper.of(null);
        BooleanWrapper b = BooleanWrapper.of(null);
        BooleanWrapper result = a.or(b);
        assertNull(result.get(), "Expected null because both values are null");
    }

    /**
     * Test for xor() method
     */
    @Test
    void xor_shouldReturnFalse_whenBothValuesAreTrue() {
        BooleanWrapper a = BooleanWrapper.of(true);
        BooleanWrapper b = BooleanWrapper.of(true);
        BooleanWrapper result = a.xor(b);
        assertNotNull(result.get(), "Expected non-null result");
        assertFalse(result.get(), "Expected false because true XOR true is false");
    }

    @Test
    void xor_shouldReturnFalse_whenBothValuesAreFalse() {
        BooleanWrapper a = BooleanWrapper.of(false);
        BooleanWrapper b = BooleanWrapper.of(false);
        BooleanWrapper result = a.xor(b);
        assertNotNull(result.get(), "Expected non-null result");
        assertFalse(result.get(), "Expected false because false XOR false is false");
    }

    @Test
    void xor_shouldReturnTrue_whenValuesAreDifferent() {
        BooleanWrapper a = BooleanWrapper.of(true);
        BooleanWrapper b = BooleanWrapper.of(false);
        BooleanWrapper result = a.xor(b);
        assertNotNull(result.get(), "Expected non-null result");
        assertTrue(result.get(), "Expected true because true XOR false is true");

        BooleanWrapper c = BooleanWrapper.of(false);
        BooleanWrapper d = BooleanWrapper.of(true);
        BooleanWrapper result2 = c.xor(d);
        assertNotNull(result2.get(), "Expected non-null result");
        assertTrue(result2.get(), "Expected true because false XOR true is true");
    }

    @Test
    void xor_shouldReturnNull_whenFirstValueIsNull() {
        BooleanWrapper a = BooleanWrapper.of(null);
        BooleanWrapper b = BooleanWrapper.of(true);
        BooleanWrapper result = a.xor(b);
        assertNull(result.get(), "Expected null because the first value is null");

        BooleanWrapper c = BooleanWrapper.of(null);
        BooleanWrapper d = BooleanWrapper.of(false);
        BooleanWrapper result2 = c.xor(d);
        assertNull(result2.get(), "Expected null because the first value is null");
    }

    @Test
    void xor_shouldReturnNull_whenSecondValueIsNull() {
        BooleanWrapper a = BooleanWrapper.of(true);
        BooleanWrapper b = BooleanWrapper.of(null);
        BooleanWrapper result = a.xor(b);
        assertNull(result.get(), "Expected null because the second value is null");

        BooleanWrapper c = BooleanWrapper.of(false);
        BooleanWrapper d = BooleanWrapper.of(null);
        BooleanWrapper result2 = c.xor(d);
        assertNull(result2.get(), "Expected null because the second value is null");
    }

    @Test
    void xor_shouldReturnNull_whenBothValuesAreNull() {
        BooleanWrapper a = BooleanWrapper.of(null);
        BooleanWrapper b = BooleanWrapper.of(null);
        BooleanWrapper result = a.xor(b);
        assertNull(result.get(), "Expected null because both values are null");
    }

    @Test
    void xor_shouldReturnNull_whenOtherIsNull() {
        BooleanWrapper a = BooleanWrapper.of(true);
        BooleanWrapper result = a.xor(null);
        assertNull(result.get(), "Expected null because the other BooleanWrapper is null");

        BooleanWrapper b = BooleanWrapper.of(false);
        BooleanWrapper result2 = b.xor(null);
        assertNull(result2.get(), "Expected null because the other BooleanWrapper is null");
    }

    /**
     * Test for not() method
     */
    @Test
    void not_shouldReturnFalse_whenValueIsTrue() {
        BooleanWrapper a = BooleanWrapper.of(true);
        BooleanWrapper result = a.not();
        assertNotNull(result.get(), "Expected non-null result");
        assertFalse(result.get(), "Expected false because NOT true is false");
    }

    @Test
    void not_shouldReturnTrue_whenValueIsFalse() {
        BooleanWrapper a = BooleanWrapper.of(false);
        BooleanWrapper result = a.not();
        assertNotNull(result.get(), "Expected non-null result");
        assertTrue(result.get(), "Expected true because NOT false is true");
    }

    @Test
    void not_shouldReturnNull_whenValueIsNull() {
        BooleanWrapper a = BooleanWrapper.of(null);
        BooleanWrapper result = a.not();
        assertNull(result.get(), "Expected null because NOT of a null value should return null");
    }

    /**
     * Test for map() method
     */
    @Test
    void map_shouldApplyFunction_whenValueIsTrue() {
        BooleanWrapper wrapper = BooleanWrapper.of(true);
        Optional<String> result = wrapper.map(value -> value ? "YES" : "NO");

        assertTrue(result.isPresent(), "Expected non-empty Optional because value is not null");
        assertEquals("YES", result.get(), "Expected 'YES' because value is true");
    }

    @Test
    void map_shouldApplyFunction_whenValueIsFalse() {
        BooleanWrapper wrapper = BooleanWrapper.of(false);
        Optional<String> result = wrapper.map(value -> value ? "YES" : "NO");

        assertTrue(result.isPresent(), "Expected non-empty Optional because value is not null");
        assertEquals("NO", result.get(), "Expected 'NO' because value is false");
    }

    @Test
    void map_shouldReturnEmptyOptional_whenValueIsNull() {
        BooleanWrapper wrapper = BooleanWrapper.of(null);
        Optional<String> result = wrapper.map(value -> value ? "YES" : "NO");

        assertTrue(result.isEmpty(), "Expected an empty Optional because value is null");
    }

    @Test
    void map_shouldThrowException_whenMapperIsNull() {
        BooleanWrapper wrapper = BooleanWrapper.of(true);

        Exception exception = assertThrows(IllegalArgumentException.class,
                () -> wrapper.map(null));

        assertEquals("mapping function cannot be null",
                exception.getMessage(),
                "Expected IllegalArgumentException due to null mapper function");
    }

    /**
     * Test for toPrimitive() method
     */
    @Test
    void toPrimitive_shouldReturnTrue_whenValueIsTrue() {
        BooleanWrapper wrapper = BooleanWrapper.of(true);
        assertTrue(wrapper.toPrimitive(), "Expected true because the wrapped value is true");
    }

    @Test
    void toPrimitive_shouldReturnFalse_whenValueIsFalse() {
        BooleanWrapper wrapper = BooleanWrapper.of(false);
        assertFalse(wrapper.toPrimitive(), "Expected false because the wrapped value is false");
    }

    @Test
    void toPrimitive_shouldReturnFalse_whenValueIsNull() {
        BooleanWrapper wrapper = BooleanWrapper.of(null);
        assertFalse(wrapper.toPrimitive(), "Expected false because the wrapped value is null, using false as default");
    }
}