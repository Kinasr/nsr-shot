package kinasr.nsr_shot.model;

import kinasr.nsr_shot.utility.config.ConfigHandler;

public class ShotOption {
    private boolean resizeImage;
    private boolean forceResizeWindow;
    private boolean supportFluent;
    private int fluentDepth;

    public ShotOption(boolean resizeImage, boolean forceResizeWindow, boolean supportFluent, int fluentDepth) {
        this.resizeImage = resizeImage;
        this.forceResizeWindow = forceResizeWindow;
        this.supportFluent = supportFluent;
        this.fluentDepth = fluentDepth;
    }

    public static ShotOption create() {
        return new ShotOption(
                ConfigHandler.resizeImage(),
                ConfigHandler.forceResizeWindow(),
                ConfigHandler.supportFluentNaming(),
                ConfigHandler.supportFluentNaming() ? 4 : 3
        );
    }

    public Boolean resizeImage() {
        return resizeImage;
    }

    public ShotOption resizeImage(Boolean resizeImage) {
        this.resizeImage = resizeImage;
        return this;
    }

    public Boolean forceResizeWindow() {
        return forceResizeWindow;
    }

    public ShotOption forceResizeWindow(Boolean forceResizeWindow) {
        this.forceResizeWindow = forceResizeWindow;
        return this;
    }

    public Boolean supportFluent() {
        return supportFluent;
    }

    public ShotOption supportFluent(Boolean supportFluent) {
        this.supportFluent = supportFluent;
        return this;
    }

    public Integer fluentDepth() {
        return fluentDepth;
    }

    public ShotOption fluentDepth(Integer fluentDepth) {
        this.fluentDepth = fluentDepth;
        return this;
    }
}
