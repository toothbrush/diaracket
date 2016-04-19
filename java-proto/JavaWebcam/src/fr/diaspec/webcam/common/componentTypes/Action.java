package fr.diaspec.webcam.common.componentTypes;

import fr.diaspec.webcam.common.Initialisable;
import fr.diaspec.webcam.common.Subscriber;

public interface Action<T> extends Component, Initialisable, Subscriber<T> {
	
	
}