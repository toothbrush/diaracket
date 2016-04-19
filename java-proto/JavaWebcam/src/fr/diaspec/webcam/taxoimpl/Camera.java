package fr.diaspec.webcam.taxoimpl;

import android.content.Intent;
import android.graphics.Bitmap;
import android.os.Bundle;
import android.provider.MediaStore;
import android.util.Log;
import fr.diaspec.webcam.common.CommonRuncode;
import fr.diaspec.webcam.generated.AbstractCamera;

public class Camera extends AbstractCamera {

	public static final int REQUEST_TAKE_PHOTO = 1;
	
	private Bitmap lastValue = null;

	@Override
	public void init(CommonRuncode a) {
		this.runner = a ;
		doTakePicture();
	}
	private CommonRuncode runner;

	protected void doTakePicture() {

		Log.i("gc", "dispatching picture request.");
		Intent takePictureIntent = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
		// Ensure that there's a camera activity to handle the intent
		if (takePictureIntent.resolveActivity(this.runner.getPackageManager()) != null) {

			this.runner.startActivityForResult(takePictureIntent, REQUEST_TAKE_PHOTO);

		} else {
			// we don't have permission? hm no. unrelated.
			Log.i("","[ERROR!] We cannot take pictures.");
		}

	}

	@Override
	public void pingBack(int requestCode, Intent data) {

		if (requestCode == Camera.REQUEST_TAKE_PHOTO) {
			// if we set the MediaStore.EXTRA_OUTPUT, intent == null.
			
			Log.i("gc", "picture received.");

			Bundle extras = data.getExtras();
			Bitmap imageBitmap = (Bitmap) extras.get("data");
			this.lastValue = imageBitmap;
			
			notify(imageBitmap);
		}
	}

	@Override
	protected Bitmap getCameraValue() {
		
		return this.lastValue;
	}


}
