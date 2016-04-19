package org.denknerd.evilwebcam;

import static org.denknerd.evilwebcam.U.l;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

import android.app.Activity;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.ColorMatrix;
import android.graphics.ColorMatrixColorFilter;
import android.net.Uri;
import android.os.Bundle;
import android.os.Environment;
import android.provider.MediaStore;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.widget.Button;
import android.widget.ImageView;

public class MainActivity extends Activity {

    public final static String EXTRA_MESSAGE = "org.denknerd.evilwebcam.PHOTOPATH";
    public final static String MYFILENAME = "paul_filename";
    
    private static final int REQUEST_TAKE_PHOTO = 1;

    private String mCurrentPhotoPath;

	private ImageView mImageView;
    
	private int targetW = 800, targetH = 800;
    
    
    private void connectButtons() {
    	mImageView = (ImageView)findViewById(R.id.imageView1);
    	Button button = (Button) findViewById(R.id.btnTakePic);
    	button.setOnTouchListener(new OnTouchListener() {

    		@Override
    		public boolean onTouch(View arg0, MotionEvent arg1) {

    			l(""+arg1);
    			if (arg1.getAction() != MotionEvent.ACTION_UP) {
    				// block duplicate touches
					return false;
				}
    			dispatchTakePictureIntent();
    			arg0.performClick();
    			return false;
    		}

    	});
    	
    	button = (Button) findViewById(R.id.btnSeeAdd);
    	button.setOnTouchListener(new OnTouchListener() {
			
			@Override
			public boolean onTouch(View v, MotionEvent event) {
				if (event.getAction() != MotionEvent.ACTION_UP) {
    				// block duplicate touches
					return false;
				}
				doSeeAd();
				v.performClick();
				return false;
			}
		});
    }

    private File createImageFile() throws IOException {
        // Create an image file name
        String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.FRENCH).format(new Date());
        String imageFileName = "JPEG_" + timeStamp + "_";
        File storageDir = Environment.getExternalStoragePublicDirectory(
                Environment.DIRECTORY_PICTURES);
        File image = File.createTempFile(
            imageFileName,  /* prefix */
            ".jpg",         /* suffix */
            storageDir      /* directory */
        );

        // Save a file: path for use with ACTION_VIEW intents
        mCurrentPhotoPath =           image.getAbsolutePath();
        l("mCurrentPhotoPath = " + mCurrentPhotoPath);
        return image;
    }
    
    private void dispatchTakePictureIntent() {
    	
    	Intent takePictureIntent = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
        // Ensure that there's a camera activity to handle the intent
        if (takePictureIntent.resolveActivity(getPackageManager()) != null) {
            // Create the File where the photo should go
            File photoFile = null;
            try {
                photoFile = createImageFile();
            } catch (IOException ex) {
                // Error occurred while creating the File
            	l("ioexception. probably due to missing WRITE_SD privilege.");
            	return;
            }
            // Continue only if the File was successfully created
            if (photoFile != null) {
                takePictureIntent.putExtra(MediaStore.EXTRA_OUTPUT, Uri.fromFile(photoFile));
                startActivityForResult(takePictureIntent, REQUEST_TAKE_PHOTO);
            }
        } else {
        	// we don't have permission? hm no. unrelated.
        	l("we cannot take pictures.");
        }
    }
    public void doSeeAd() {
		Intent intent = new Intent(this, ShowAdActivity.class);
		intent.putExtra(EXTRA_MESSAGE, mCurrentPhotoPath);
	    startActivity(intent);
	}
    
    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == REQUEST_TAKE_PHOTO && resultCode == RESULT_OK) {
        	// because we set the MediaStore.EXTRA_OUTPUT, intent == null.
        	setPic();
        }
    }
    
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        
        setContentView(R.layout.activity_main);
        connectButtons();
        
        if (savedInstanceState != null) {
        	if (savedInstanceState.getString(MYFILENAME) != null
        			&& savedInstanceState.getString(MYFILENAME) != "") {
				mCurrentPhotoPath = savedInstanceState.getString(MYFILENAME);
				setPic();
			}
		}
    }

    @Override
	protected void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		
		if (mCurrentPhotoPath != null) {
			outState.putString(MYFILENAME, mCurrentPhotoPath);
		}
	}

    private void setPic() {
    	l("in setPic(): mImageView = " + mImageView);
    	
    	Bitmap bitmap;
    	  
        mImageView.getLayoutParams().width  = targetW;
        mImageView.getLayoutParams().height = targetH;

        // Get the dimensions of the bitmap
        BitmapFactory.Options bmOptions = new BitmapFactory.Options();
        bmOptions.inJustDecodeBounds = true;
        l("decode file -> " + mCurrentPhotoPath);
        
        BitmapFactory.decodeFile(mCurrentPhotoPath, bmOptions);
        int photoW = bmOptions.outWidth;
        int photoH = bmOptions.outHeight;

        // Determine how much to scale down the image
        int scaleFactor = Math.min(photoW/targetW, photoH/targetH);

        // Decode the image file into a Bitmap sized to fill the View
        bmOptions.inJustDecodeBounds = false;
        bmOptions.inSampleSize = scaleFactor;
        bmOptions.inPurgeable = true;

        bitmap = BitmapFactory.decodeFile(mCurrentPhotoPath, bmOptions);

        /* here we filter the image a bit.
         */
        
        // first turn into b/w
        final ColorMatrix matrixA = new ColorMatrix();
        matrixA.setSaturation(0);

        // add sepia scaling
        final ColorMatrix matrixB = new ColorMatrix();
        matrixB.setScale(1f, 0.95f, 0.82f, 1.0f);
        matrixA.setConcat(matrixB, matrixA);

        final ColorMatrixColorFilter filter = new ColorMatrixColorFilter(matrixA);
        mImageView.setImageBitmap(bitmap);
        mImageView.getDrawable().setColorFilter(filter);
     
    }
    
}
