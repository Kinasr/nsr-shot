package kinasr.nsr_shot.utility;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertThrows;

class HelperTest {

    @Nested
    class SeparateFullNameTest {
        @Test
        void separateValidFullName() {
            var name = "integration_test.IntegrationTest#checkLoginDialogIsVisuallyCorrect_ref_1200x1183.png";

            var model = Helper.separateFullName(name);

            assertThat(model.name())
                    .isEqualTo("integration_test.IntegrationTest#checkLoginDialogIsVisuallyCorrect");
            assertThat(model.timestamp()).isEqualTo("ref");
            assertThat(model.width()).isEqualTo(1200);
            assertThat(model.height()).isEqualTo(1183);
            assertThat(model.extension()).isEqualTo(".png");
        }

        @Test
        void separateFullNameWithUnderscore() {
            var name = "integration_test.IntegrationTest#checkLoginDialogIs_Visually_Correct_ref_1200x1183.png";

            var model = Helper.separateFullName(name);

            assertThat(model.name())
                    .isEqualTo("integration_test.IntegrationTest#checkLoginDialogIs_Visually_Correct");
            assertThat(model.timestamp()).isEqualTo("ref");
            assertThat(model.width()).isEqualTo(1200);
            assertThat(model.height()).isEqualTo(1183);
            assertThat(model.extension()).isEqualTo(".png");
        }

        @Test
        void separateFullNameWithoutExtension() {
            var name = "integration_test.IntegrationTest#checkLoginDialogIsVisuallyCorrect_ref_1200x1183";

            var model = Helper.separateFullName(name);


            assertAll(() -> assertThat(model.name())
                            .isEqualTo("integration_test.IntegrationTest#checkLoginDialogIsVisuallyCorrect"),
                    () -> assertThat(model.timestamp()).isEqualTo("ref"),
                    () -> assertThat(model.width()).isEqualTo(1200),
                    () -> assertThat(model.height()).isEqualTo(1183),
                    () -> assertThat(model.extension()).isEqualTo(".png")
            );
        }

        @Test
        void separateFullNameWithoutWindowSize() {
            var name = "integration_test.IntegrationTest#checkLoginDialogIsVisuallyCorrect_ref.png";

            var model = Helper.separateFullName(name);

            assertThat(model.name())
                    .isEqualTo("integration_test.IntegrationTest#checkLoginDialogIsVisuallyCorrect");
            assertThat(model.timestamp()).isEqualTo("ref");
            assertThat(model.windowSize()).isEmpty();
            assertThat(model.extension()).isEqualTo(".png");
        }

        @Test
        void separateFullNameWithoutTimestamp() {
            var name = "integration_test.IntegrationTest#checkLoginDialogIsVisuallyCorrect_1200x1183.png";

            var model = Helper.separateFullName(name);

            assertThat(model.name())
                    .isEqualTo("integration_test.IntegrationTest#checkLoginDialogIsVisuallyCorrect");
            assertThat(model.timestamp()).isEqualTo("");
            assertThat(model.width()).isEqualTo(1200);
            assertThat(model.height()).isEqualTo(1183);
            assertThat(model.extension()).isEqualTo(".png");
        }

        @Test
        void separateFullNameWithoutName() {
            var name = "ref_1200x1183.png";

            var model = Helper.separateFullName(name);

            assertThat(model.name()).isEmpty();
            assertThat(model.timestamp()).isEqualTo("ref");
            assertThat(model.width()).isEqualTo(1200);
            assertThat(model.height()).isEqualTo(1183);
            assertThat(model.extension()).isEqualTo(".png");
        }

        @Test
        void separateFullNameWithXInTheName() {
            var name = "integration_test.IntegrationTest#checkLoginDialogIsVisuallyxCorrect_ref_1200x1183.png";

            var model = Helper.separateFullName(name);

            assertThat(model.name())
                    .isEqualTo("integration_test.IntegrationTest#checkLoginDialogIsVisuallyxCorrect");
            assertThat(model.timestamp()).isEqualTo("ref");
            assertThat(model.width()).isEqualTo(1200);
            assertThat(model.height()).isEqualTo(1183);
            assertThat(model.extension()).isEqualTo(".png");
        }

        @Test
        void separateFullNameWithNumbers() {
            var name = "integration_test.IntegrationTest#checkLoginDialogIsVisually0Correct1_ref_1200x1183.png";

            var model = Helper.separateFullName(name);

            assertThat(model.name())
                    .isEqualTo("integration_test.IntegrationTest#checkLoginDialogIsVisually0Correct1");
            assertThat(model.timestamp()).isEqualTo("ref");
            assertThat(model.width()).isEqualTo(1200);
            assertThat(model.height()).isEqualTo(1183);
            assertThat(model.extension()).isEqualTo(".png");
        }

        @Test
        void separateEmptyFullName() {
            assertThrows(
                    IllegalArgumentException.class,
                    () -> Helper.separateFullName("")
            );
        }

        @Test
        void separateNullFullName() {
            assertThrows(
                    IllegalArgumentException.class,
                    () -> Helper.separateFullName(null)
            );
        }
    }
}