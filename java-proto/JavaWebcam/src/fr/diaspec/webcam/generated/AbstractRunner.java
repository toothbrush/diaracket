package fr.diaspec.webcam.generated;

import fr.diaspec.webcam.common.CommonRuncode;
import fr.diaspec.webcam.taxoimpl.Camera;
import fr.diaspec.webcam.taxoimpl.IP;
import fr.diaspec.webcam.taxoimpl.Screen;

public abstract class AbstractRunner extends CommonRuncode
{
  @Override
  protected void init ()
  {
    display_ = getDisplayInstance();
    display_.init(this);
    ctrls.add(display_);
    makead_ = getMakeAdInstance();
    makead_.init(this);
    ctxs.add(makead_);
    composedisplay_ = getComposeDisplayInstance();
    composedisplay_.init(this);
    ctxs.add(composedisplay_);
    processpicture_ = getProcessPictureInstance();
    processpicture_.init(this);
    ctxs.add(processpicture_);
    acts.add(screen_);
    srcs.add(camera_);
    srcs.add(ip_);
    composedisplay_.addSubscriber(display_);
    processpicture_.addSubscriber(composedisplay_);
    camera_.addSubscriber(processpicture_);
  }
  private AbstractDisplay display_;
  private AbstractMakeAd makead_;
  private AbstractComposeDisplay composedisplay_;
  private AbstractProcessPicture processpicture_;
  private static final AbstractScreen screen_ = new Screen();
  private static final AbstractCamera camera_ = new Camera();
  private static final AbstractIP ip_ = new IP();
  public abstract AbstractDisplay getDisplayInstance ()
  ;
  public abstract AbstractMakeAd getMakeAdInstance ()
  ;
  public abstract AbstractComposeDisplay getComposeDisplayInstance ()
  ;
  public abstract AbstractProcessPicture getProcessPictureInstance ()
  ;
  public AbstractScreen getScreenInstance ()
  {
    return screen_;
  }
  public AbstractCamera getCameraInstance ()
  {
    return camera_;
  }
  public AbstractIP getIPInstance ()
  {
    return ip_;
  }
}