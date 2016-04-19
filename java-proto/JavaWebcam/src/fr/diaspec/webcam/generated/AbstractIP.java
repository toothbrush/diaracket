package fr.diaspec.webcam.generated;

import fr.diaspec.webcam.common.Publisher;
import fr.diaspec.webcam.common.componentTypes.Source;

public abstract class AbstractIP extends Publisher<String> implements Source<String>
{
  protected abstract String getIPValue ()
  ;
  public String requireValue ()
  {
    return getIPValue();
  }
}