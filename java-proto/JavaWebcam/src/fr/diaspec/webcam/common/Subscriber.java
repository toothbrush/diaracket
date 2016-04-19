package fr.diaspec.webcam.common;

public interface Subscriber<T> {

    public abstract void trigger(T value);

}