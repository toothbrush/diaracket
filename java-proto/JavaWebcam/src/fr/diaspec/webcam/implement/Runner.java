package fr.diaspec.webcam.implement;

import android.os.Bundle;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.widget.Button;
import fr.diaspec.webcam.R;
import fr.diaspec.webcam.generated.AbstractMakeAd;
import fr.diaspec.webcam.generated.AbstractComposeDisplay;
import fr.diaspec.webcam.generated.AbstractProcessPicture;
import fr.diaspec.webcam.generated.AbstractRunner;
import fr.diaspec.webcam.generated.AbstractDisplay;


public class Runner extends AbstractRunner {

	
	private void connectButtons() {
    
		// this plumbing calls the framework "run()" method if 
		// necessary.
    	Button button = (Button) findViewById(R.id.takePicture);
    	button.setOnTouchListener(new OnTouchListener() {
    		@Override
    		public boolean onTouch(View arg0, MotionEvent arg1) {

    			if (arg1.getAction() != MotionEvent.ACTION_UP) {
    				// block duplicate touches
					return false;
				}
    			run(); // call the framework run() method.
    			arg0.performClick();
    			return false;
    		}

    	});
    }
   
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_main);
		
		Log.i("goodcam", "developer's implementation started.");
        
		connectButtons();   // connect button we made to the
							// framework's run() method.
		
	}
	
	// these need to be class fields so that the instances are long-lived.
    private AbstractMakeAd ad;
    private AbstractProcessPicture mp;
    private AbstractComposeDisplay as;
    private AbstractDisplay sc;
    
	@Override
	public AbstractProcessPicture getProcessPictureInstance() {
		if(mp==null)
			mp = new ProcessPicture();
		return mp;
	}

	@Override
	public AbstractMakeAd getMakeAdInstance() {
		if(ad == null)
			ad = new MakeAd();
		return ad;
	}

	@Override
	public AbstractComposeDisplay getComposeDisplayInstance() {
		if(as == null)
			as = new ComposeDisplay();
		return as;
	}

	@Override
	public AbstractDisplay getDisplayInstance() {
		if(sc== null)
			sc = new Display();
		return sc;
	}

}