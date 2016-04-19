package fr.diaspec.webcam.taxoimpl;

import android.graphics.Bitmap;
import android.util.Log;
import android.widget.ImageView;
import fr.diaspec.webcam.R;
import fr.diaspec.webcam.common.CommonRuncode;
import fr.diaspec.webcam.generated.AbstractScreen;

public class Screen extends AbstractScreen {

	private CommonRuncode runner;

	@Override
	protected void doScreenAction(Bitmap newVisual) {

		if(this.runner == null) {
			throw new RuntimeException("Calling action before initialisation!");
		}
		
		Log.i("goodcam", "received a bitmap = " + newVisual);
		
		ImageView iv = (ImageView)this.runner.findViewById(R.id.imageView1);
		
		iv.setImageBitmap(newVisual);
		
	}

	@Override
	public void init(CommonRuncode a) {
		this.runner = a;
	}
}