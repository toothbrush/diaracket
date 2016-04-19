package fr.diaspec.webcam.implement;

import android.graphics.Bitmap;
import android.util.Log;
import fr.diaspec.webcam.generated.AbstractDisplay;

public class Display extends AbstractDisplay {


	@Override
	protected void onComposeDisplayProvided(Bitmap newValue,
			ScreenProxy localScreenProxy) {
		Log.i("goodcam", "Display [controller]: updating screen to new bitmap. " + newValue);
		localScreenProxy.doScreenAction(newValue);
	}
}
