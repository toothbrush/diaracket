package fr.diaspec.webcam.implement;

import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.ColorMatrix;
import android.graphics.ColorMatrixColorFilter;
import android.graphics.Paint;
import android.util.Log;
import fr.diaspec.webcam.generated.AbstractProcessPicture;

public class ProcessPicture extends AbstractProcessPicture {

	@Override
	protected Bitmap onCameraProvided(Bitmap pictureProvided) {
	
		Log.i("", "make sepia from " + pictureProvided);
		Bitmap mod = sepia(pictureProvided);
		
		return mod;
	}
	
	private Bitmap sepia(Bitmap src){
		int width = src.getWidth();
		int height = src.getHeight();

		Bitmap dest = Bitmap.createBitmap(width, height,
				Bitmap.Config.RGB_565);

		Canvas canvas = new Canvas(dest);
		Paint paint = new Paint();

		// first turn into b/w
		final ColorMatrix matrixA = new ColorMatrix();
		matrixA.setSaturation(0);

		// add sepia scaling
		final ColorMatrix matrixB = new ColorMatrix();
		matrixB.setScale(1f, 0.95f, 0.82f, 1.0f);
		matrixA.setConcat(matrixB, matrixA);

		final ColorMatrixColorFilter filter = new ColorMatrixColorFilter(matrixA);

		paint.setColorFilter(filter); 
		canvas.drawBitmap(src, 0, 0, paint);

		return dest;
	}
}