package fr.diaspec.webcam.common.componentTypes;

import fr.diaspec.webcam.common.Initialisable;
import android.content.Intent;

public interface Source<T> extends Component, Initialisable {
	
	 
    // here's an Android-specific thing: a way for the Runner to talk to sources. 
    // sources frequently send out a "request" (i.e. Intent) to get hold of data (e.g.
    // the Camera), but the main Activity (in this case (Abstract)Runner gets the 
    // data back from the OS...
    
    public void pingBack(int requestCode, Intent data);
    
    public T requireValue();
}