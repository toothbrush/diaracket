package fr.diaspec.webcam.common;

import java.util.LinkedList;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import fr.diaspec.webcam.common.componentTypes.Action;
import fr.diaspec.webcam.common.componentTypes.Context;
import fr.diaspec.webcam.common.componentTypes.Controller;
import fr.diaspec.webcam.common.componentTypes.Source;

/*
 * Only put code here with works for ANY scenario. This saves having
 * to generate it in AbstractRunner.
 */
public abstract class CommonRuncode extends Activity {

	// for now, the types Context, Source, etc. are phantom types just to be
	// sure we have the right types of objects in these lists.
    protected final LinkedList<Context>    ctxs  = new LinkedList<Context>();
    protected final LinkedList<Source<?>>  srcs  = new LinkedList<Source<?>>();
    protected final LinkedList<Controller> ctrls = new LinkedList<Controller>();
    protected final LinkedList<Action<?>>  acts  = new LinkedList<Action<?>>();
    
    // an ugly hack to deal with the Camera not receiving the Intent.
    // basically, broadcast received Intents to all source components.
	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		Log.i("Runner", "intent received");
		
		if (resultCode == RESULT_OK) {
			for (Source<?> source : srcs) {
				source.pingBack(requestCode, data);
			}
		}
	}

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		init();
	}

	abstract protected void init();
	
	@Override
	public void onBackPressed() {
		Log.i("onbackpressed", "killing app");
	    android.os.Process.killProcess(android.os.Process.myPid());
	    super.onBackPressed();
	}
	
	public void run() {

		// initialise the actions and sources.
		// only sources are active, so by initialising
		// them, inevitably at some point one will publish a value.
    	Log.i("gc","starting run()");
    	 
        for (Action<?> action : acts) {
			action.init(this);
		}
        for (Source<?> source : srcs) {
            source.init(this);
        }
       
    }

	
}
