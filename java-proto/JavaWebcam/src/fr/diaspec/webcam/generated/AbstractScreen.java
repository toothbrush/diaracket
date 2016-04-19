package fr.diaspec.webcam.generated;

import android.graphics.Bitmap;
import fr.diaspec.webcam.common.componentTypes.Action;

public abstract class AbstractScreen implements Action<Bitmap>
{
  protected abstract void doScreenAction (Bitmap value)
  ;
  public void trigger (Bitmap value)
  {
    doScreenAction(value);
  }
}