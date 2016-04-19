package fr.diaspec.webcam.common;


public interface Initialisable {
	// sources are the only active (As opposed to reactive) components, and 
	// may therefore spawn threads with which they may later fire actions.
	// this is why we give them an init() possibility.
	// they should also have access to the main application "Activity", or View.
	public void init(CommonRuncode a);

}
