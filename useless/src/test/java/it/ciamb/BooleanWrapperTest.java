package it.ciamb;

import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.*;
class BooleanWrapperTest {

    /**
     * Test for of() method
     */
    @Test
    void of_trueTestCase() {
        BooleanWrapper wrapper = BooleanWrapper.of(true);
        assertNotNull(wrapper);
        assertTrue(wrapper.isTrue());
        assertFalse(wrapper.isFalse());
        assertTrue(wrapper.isPresent());
        assertEquals(Boolean.TRUE, wrapper.get());
    }

    @Test
    void of_falseTestCase() {
        BooleanWrapper wrapper = BooleanWrapper.of(false);
        assertNotNull(wrapper);
        assertFalse(wrapper.isTrue());
        assertTrue(wrapper.isFalse());
        assertTrue(wrapper.isPresent());
        assertEquals(Boolean.FALSE, wrapper.get());
    }

    @Test
    void of_nullTestCase() {
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
    void isTrue_trueTestCase() {
        BooleanWrapper wrapper = BooleanWrapper.of(true);
        assertTrue(wrapper.isTrue(), "Must be true for true value");
    }

    @Test
    void isTrue_falseTestCase() {
        BooleanWrapper wrapper = BooleanWrapper.of(false);
        assertFalse(wrapper.isTrue(), "Must be false for false value");
    }

    @Test
    void isTrue_nullTestCase() {
        BooleanWrapper wrapper = BooleanWrapper.of(null);
        assertFalse(wrapper.isTrue(), "Must be false for null value");
    }

    /**
     * Test for isFalse() method
     */
    @Test
    void isFalse_trueTestCase() {
        BooleanWrapper wrapper = BooleanWrapper.of(true);
        assertFalse(wrapper.isFalse(), "Must be false for true value");
    }

    @Test
    void isFalse_falseTestCase() {
        BooleanWrapper wrapper = BooleanWrapper.of(false);
        assertTrue(wrapper.isFalse(), "Must be true for false value");
    }

    @Test
    void isFalse_nullTestCase() {
        BooleanWrapper wrapper = BooleanWrapper.of(null);
        assertFalse(wrapper.isFalse(), "Must be false for null value");
    }

    /**
     * Test for isPresent() method
     */
    @Test
    void isPresent_trueTestCase() {
        BooleanWrapper wrapper = BooleanWrapper.of(true);
        assertTrue(wrapper.isPresent(), "Must be true for true value");
    }

    @Test
    void isPresent_falseTestCase() {
        BooleanWrapper wrapper = BooleanWrapper.of(false);
        assertTrue(wrapper.isPresent(), "Must be true for false value");
    }

    @Test
    void isPresent_nullTestCase() {
        BooleanWrapper wrapper = BooleanWrapper.of(null);
        assertFalse(wrapper.isPresent(), "Must be false for null value");
    }

    /**
     * Test for ifTrue() method
     */
    @Test
    void ifTrue_withTrueValue() {
        AtomicBoolean wasExecuted = new AtomicBoolean(false);

        BooleanWrapper wrapper = BooleanWrapper.of(true);
        wrapper.ifTrue(() -> wasExecuted.set(true));

        assertTrue(wasExecuted.get(), "Expected action to be executed when value is true");
    }

    @Test
    void ifTrue_withFalseValue() {
        AtomicBoolean wasExecuted = new AtomicBoolean(false);

        BooleanWrapper wrapper = BooleanWrapper.of(false);
        wrapper.ifTrue(() -> wasExecuted.set(true));

        assertFalse(wasExecuted.get(), "Expected action NOT to be executed when value is false");
    }

    @Test
    void ifTrue_withNullValue() {
        AtomicBoolean wasExecuted = new AtomicBoolean(false);

        BooleanWrapper wrapper = BooleanWrapper.of(null);
        wrapper.ifTrue(() -> wasExecuted.set(true));

        assertFalse(wasExecuted.get(), "Expected action NOT to be executed when value is null");
    }

    @Test
    void ifTrue_withNullAction() {
        BooleanWrapper wrapper = BooleanWrapper.of(true);

        assertThrows(IllegalArgumentException.class,
                () -> wrapper.ifTrue(null),
                "Expected IllegalArgumentException when action is null");
    }

    /**
     * Test for ifFalse() method
     */
    @Test
    void ifFalse_withFalseValue() {
        AtomicBoolean wasExecuted = new AtomicBoolean(false);

        BooleanWrapper wrapper = BooleanWrapper.of(false);
        wrapper.ifFalse(() -> wasExecuted.set(true));

        assertTrue(wasExecuted.get(), "Expected action to be executed when value is false");
    }

    @Test
    void ifFalse_withTrueValue() {
        AtomicBoolean wasExecuted = new AtomicBoolean(false);

        BooleanWrapper wrapper = BooleanWrapper.of(true);
        wrapper.ifFalse(() -> wasExecuted.set(true));

        assertFalse(wasExecuted.get(), "Expected action NOT to be executed when value is true");
    }

    @Test
    void ifFalse_withNullValue() {
        AtomicBoolean wasExecuted = new AtomicBoolean(false);

        BooleanWrapper wrapper = BooleanWrapper.of(null);
        wrapper.ifFalse(() -> wasExecuted.set(true));

        assertFalse(wasExecuted.get(), "Expected action NOT to be executed when value is null");
    }

    @Test
    void ifFalse_withNullAction() {
        BooleanWrapper wrapper = BooleanWrapper.of(false);

        assertThrows(IllegalArgumentException.class,
                () -> wrapper.ifFalse(null),
                "Expected IllegalArgumentException when action is null");
    }

    /**
     * Test for ifPresent() method
     */
    @Test
    void ifPresent_withTrueValue() {
        AtomicBoolean wasExecuted = new AtomicBoolean(false);

        BooleanWrapper wrapper = BooleanWrapper.of(true);
        wrapper.ifPresent(value -> wasExecuted.set(true));

        assertTrue(wasExecuted.get(), "Expected action to be executed when value is present");
    }

    @Test
    void ifPresent_withFalseValue() {
        AtomicBoolean wasExecuted = new AtomicBoolean(false);

        BooleanWrapper wrapper = BooleanWrapper.of(false);
        wrapper.ifPresent(value -> wasExecuted.set(true));

        assertTrue(wasExecuted.get(), "Expected action to be executed when value is present");
    }

    @Test
    void ifPresent_withNullValue() {
        AtomicBoolean wasExecuted = new AtomicBoolean(false);

        BooleanWrapper wrapper = BooleanWrapper.of(null);
        wrapper.ifPresent(value -> wasExecuted.set(true));

        assertFalse(wasExecuted.get(), "Expected action NOT to be executed when value is null");
    }

    @Test
    void ifPresent_withNullAction() {
        BooleanWrapper wrapper = BooleanWrapper.of(true);

        assertThrows(IllegalArgumentException.class, 
                () -> wrapper.ifPresent(null),
                "Expected IllegalArgumentException when action is null");
    }
}