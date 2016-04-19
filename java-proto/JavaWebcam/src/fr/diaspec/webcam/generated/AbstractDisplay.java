package fr.diaspec.webcam.generated;

import android.graphics.Bitmap;
import fr.diaspec.webcam.common.Subscriber;
import fr.diaspec.webcam.common.componentTypes.Controller;

public abstract class AbstractDisplay implements Controller, Subscriber<Bitmap>
{
  protected abstract void onComposeDisplayProvided (Bitmap newValue, ScreenProxy localScreenProxy)
  ;
  public final void trigger (Bitmap value)
  {
    ScreenProxy localScreenProxy = new ScreenProxy();
    localScreenProxy.setAccessible(true);
    onComposeDisplayProvided(value, localScreenProxy);
    localScreenProxy.setAccessible(false);
  }
  protected final class ScreenProxy
  {
    private ScreenProxy ()
    {
    }
    final private void setAccessible (boolean isAccessible)
    {
      this.isAccessible = isAccessible;
    }
    private boolean isAccessible = false;
    final public void doScreenAction (Bitmap value)
    {
      if (isAccessible)
      {
        runner.getScreenInstance().trigger(value);
      }
      else
      {
        throw new RuntimeException("Access forbidden for Screen action");
      }
    }
  }
  private AbstractRunner runner;
  final protected void init (AbstractRunner runner)
  {
    this.runner = runner;
  }
}