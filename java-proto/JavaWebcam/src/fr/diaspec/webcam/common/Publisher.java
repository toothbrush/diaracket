package fr.diaspec.webcam.common;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;

public abstract class Publisher<T> {

    private List<Subscriber<T>> subscribers = new ArrayList<Subscriber<T>>();

    public void addSubscriber(Subscriber<T> subscriber) {
        subscribers.add(subscriber);
    }

    // protected because can be called in the implementation.
    protected void notify(T newvalue) {
    	Log.i("gc, Publisher", "publishing to " + subscribers);
        for (Subscriber<T> subscriber : subscribers) {
            subscriber.trigger(newvalue);
        }}}