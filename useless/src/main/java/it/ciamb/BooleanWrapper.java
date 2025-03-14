package it.ciamb;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Classe wrapper per la gestione dichiarativa dei valori booleani.
 * <p>
 * Fornisce un'API fluente per operare sui booleani in modo pi� leggibile e funzionale,
 * evitando l'uso di costrutti if-else espliciti.
 * </p>
 *
 * <h3>Esempio d'uso:</h3>
 * <pre>{@code
 * BooleanWrapper.of(true)
 *     .ifTrue(() -> System.out.println("� vero!"))
 *     .ifFalse(() -> System.out.println("� falso!"))
 *     .orElse(() -> System.out.println("� nullo!"));
 * }</pre>
 */
public class BooleanWrapper {
    private final Boolean value;

    private BooleanWrapper(Boolean value) {
        this.value = value;
    }

    /**
     * Crea un'istanza di {@code BooleanWrapper} con il valore specificato.
     *
     * @param value il valore booleano da incapsulare
     * @return un'istanza di {@code BooleanWrapper}
     */
    public static BooleanWrapper of(Boolean value) {
        return new BooleanWrapper(value);
    }

    /**
     * Verifica se il valore booleano incapsulato � {@code true}.
     *
     * @return {@code true} se il valore � {@code true}, {@code false} in caso contrario
     */
    public boolean isTrue() {
        return Boolean.TRUE.equals(value);
    }

    /**
     * Verifica se il valore booleano incapsulato � {@code false}.
     *
     * @return {@code true} se il valore � {@code false}, {@code false} in caso contrario
     */
    public boolean isFalse() {
        return Boolean.FALSE.equals(value);
    }

    /**
     * Verifica se il valore booleano � presente (non {@code null}).
     *
     * @return {@code true} se il valore � non-null, {@code false} altrimenti
     */
    public boolean isPresent() {
        return value != null;
    }

    /**
     * Esegue l'azione specificata se il valore booleano � {@code true}.
     *
     * @param action l'azione da eseguire se il valore � {@code true}
     * @return questa istanza di {@code BooleanWrapper} per il chaining
     * @throws IllegalArgumentException se {@code action} � {@code null}
     */
    public BooleanWrapper ifTrue(Runnable action) {
        if (action == null)
            throw new IllegalArgumentException("action cannot be null");
        if (Boolean.TRUE.equals(value))
            action.run();
        return this;
    }

    /**
     * Esegue l'azione specificata se il valore booleano � {@code false}.
     *
     * @param action l'azione da eseguire se il valore � {@code false}
     * @return questa istanza di {@code BooleanWrapper} per il chaining
     * @throws IllegalArgumentException se {@code action} � {@code null}
     */
    public BooleanWrapper ifFalse(Runnable action) {
        if (action == null)
            throw new IllegalArgumentException("action cannot be null");
        if (Boolean.FALSE.equals(value))
            action.run();
        return this;
    }

    /**
     * Esegue l'azione specificata se il valore booleano � presente (non {@code null}).
     *
     * @param action l'azione da eseguire con il valore booleano presente
     * @return questa istanza di {@code BooleanWrapper} per il chaining
     * @throws IllegalArgumentException se {@code action} � {@code null}
     */
    public BooleanWrapper ifPresent(Consumer<Boolean> action) {
        if (action == null)
            throw new IllegalArgumentException("action cannot be null");
        if (value != null)
            action.accept(value);
        return this;
    }

    /**
     * Esegue l'azione specificata se il valore � {@code null}.
     *
     * @param action l'azione da eseguire se il valore � {@code null}
     * @return questa istanza di {@code BooleanWrapper} per il chaining
     * @throws IllegalArgumentException se {@code action} � {@code null}
     */
    public BooleanWrapper orElse(Runnable action) {
        if (action == null)
            throw new IllegalArgumentException("action cannot be null");
        if (value == null)
            action.run();
        return this;
    }

    /**
     * Restituisce il valore booleano incapsulato o un valore di default se il valore � {@code null}.
     *
     * @param defaultValue il valore predefinito da restituire se il valore � {@code null}
     * @return il valore booleano incapsulato o {@code defaultValue} se � {@code null}
     */
    public boolean orElse(boolean defaultValue) {
        return Optional.ofNullable(value).orElse(defaultValue);
    }

    /**
     * Restituisce il valore booleano incapsulato o un valore fornito da un {@code Supplier}
     * se il valore � {@code null}.
     *
     * @param supplier il fornitore del valore booleano predefinito in caso di {@code null}
     * @return il valore booleano incapsulato o il valore fornito dal {@code Supplier} se � {@code null}
     * @throws IllegalArgumentException se {@code supplier} � {@code null}
     */
    public boolean orElseGet(Supplier<Boolean> supplier) {
        if (supplier == null)
            throw new IllegalArgumentException("supplier cannot be null");
        return value != null ? value : supplier.get();
    }

    /**
     * Esegue un'operazione di AND logico tra il valore corrente e quello di un altro {@code BooleanWrapper}.
     *
     * @param other l'altro {@code BooleanWrapper} da combinare con AND
     * @return un nuovo {@code BooleanWrapper} con il risultato dell'AND logico
     */
    public BooleanWrapper and(BooleanWrapper other) {
        if (other == null || other.value == null || this.value == null)
            return BooleanWrapper.of(null);
        return new BooleanWrapper(this.value && other.value);
    }

    /**
     * Esegue un'operazione di OR logico tra il valore corrente e quello di un altro {@code BooleanWrapper}.
     *
     * @param other l'altro {@code BooleanWrapper} da combinare con OR
     * @return un nuovo {@code BooleanWrapper} con il risultato dell'OR logico
     */
    public BooleanWrapper or(BooleanWrapper other) {
        if (other == null || other.value == null)
            return this;
        if (this.value == null)
            return other;
        return new BooleanWrapper(this.value || other.value);
    }

    /**
     * Esegue un'operazione di XOR logico tra il valore corrente e quello di un altro {@code BooleanWrapper}.
     * <p>
     * L'operazione XOR restituisce {@code true} se i due valori sono diversi, {@code false} se sono uguali.
     * Se uno dei due valori � {@code null}, viene restituito un {@code BooleanWrapper} con valore {@code null}.
     * </p>
     *
     * @param other l'altro {@code BooleanWrapper} da combinare con XOR
     * @return un nuovo {@code BooleanWrapper} con il risultato dello XOR logico
     */
    public BooleanWrapper xor(BooleanWrapper other) {
        if (other == null || other.value == null || this.value == null)
            return BooleanWrapper.of(null);
        return new BooleanWrapper(this.value ^ other.value);
    }

    /**
     * Restituisce il valore booleano invertito.
     *
     * @return un nuovo {@code BooleanWrapper} con il valore negato, oppure un {@code BooleanWrapper} con valore {@code null}
     */
    public BooleanWrapper not() {
        return value != null ? new BooleanWrapper(!value) : BooleanWrapper.of(null);
    }

    /**
     * Applica una funzione di trasformazione al valore booleano e restituisce un {@code Optional}
     * contenente il risultato.
     *
     * @param <T>    il tipo del valore trasformato
     * @param mapper la funzione di trasformazione
     * @return un {@code Optional} contenente il risultato della trasformazione,
     * o un {@code Optional.empty()} se il valore � {@code null}
     * @throws IllegalArgumentException se {@code mapper} � {@code null}
     */
    public <T> Optional<T> map(Function<Boolean, T> mapper) {
        if (mapper == null)
            throw new IllegalArgumentException("mapping function cannot be null");
        return Optional.ofNullable(value).map(mapper);
    }

    /**
     * Restituisce il valore booleano incapsulato.
     *
     * @return il valore booleano, che potrebbe essere {@code null}
     */
    public Boolean get() {
        return value;
    }

    /**
     * Restituisce il valore booleano primitivo associato a questa istanza.
     * <p>
     * Se il valore booleano � {@code null}, restituisce {@code false} come valore di default.
     * </p>
     *
     * @return il valore booleano incapsulato se presente, altrimenti {@code false}
     */
    public boolean toPrimitive() {
        return value != null && value;
    }

}
